{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty          (ScottyM, post, header, status, json, body, ActionM, scotty)
import Network.HTTP.Types.Status
import Data.Aeson hiding (json)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Simple
import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)

-- Data types for the webhook payload
data WebhookPayload = WebhookPayload
  { event          :: T.Text
  , transaction_id :: T.Text
  , amount         :: AmountValue  -- Changed to handle both string and number
  , currency       :: T.Text
  , timestamp      :: Maybe T.Text  -- Optional to handle missing field test
  } deriving (Show, Generic)

-- Custom type to handle amount as either string or number
data AmountValue = AmountValue Double deriving (Show)

instance FromJSON AmountValue where
  parseJSON (Number n) = return $ AmountValue (realToFrac n)
  parseJSON (String s) = case readMaybe (T.unpack s) of
    Just d  -> return $ AmountValue d
    Nothing -> fail "Invalid amount format"
  parseJSON _ = fail "Amount must be a number or string"

instance FromJSON WebhookPayload
instance ToJSON WebhookPayload where
  toJSON (WebhookPayload e tid (AmountValue amt) curr ts) =
    object [ "event" .= e
           , "transaction_id" .= tid
           , "amount" .= amt
           , "currency" .= curr
           , "timestamp" .= ts
           ]

-- Data type for callback payloads (field renamed to avoid clash)
data CallbackPayload = CallbackPayload
  { cb_transaction_id :: T.Text
  } deriving (Show)

-- Manually write a ToJSON instance so that JSON key is still "transaction_id"
instance ToJSON CallbackPayload where
  toJSON (CallbackPayload tid) =
    object ["transaction_id" .= tid]

-- Expected token for authentication
expectedToken :: T.Text
expectedToken = "meu-token-secreto"

-- Callback URLs
confirmUrl :: String
confirmUrl = "http://127.0.0.1:5000/confirmar"

cancelUrl :: String
cancelUrl = "http://127.0.0.1:5000/cancelar"

-- Main application
main :: IO ()
main = do
  -- Create a TVar to store processed transaction IDs
  processedTransactions <- atomically $ newTVar Set.empty

  scotty 5001 $ do
    -- Webhook endpoint
    post "/webhook" $ do
      -- Get the authentication token
      tokenHeader <- header "X-Webhook-Token"

      -- Validate token
      case tokenHeader of
        Nothing -> do
          status unauthorized401
          json $ object ["error" .= ("Missing authentication token" :: T.Text)]
        Just token ->
          if L.toStrict token /= expectedToken
          then do
            status unauthorized401
            json $ object ["error" .= ("Invalid token" :: T.Text)]
          else do
            -- Parse the request body
            bodyRaw <- body
            case decode bodyRaw :: Maybe WebhookPayload of
              Nothing -> do
                status badRequest400
                json $ object ["error" .= ("Invalid payload" :: T.Text)]
              Just payload -> do
                -- Check for duplicates FIRST
                isDuplicate <- liftIO $ atomically $ do
                  processed <- readTVar processedTransactions
                  if Set.member (transaction_id payload) processed
                    then return True
                    else do
                      writeTVar processedTransactions (Set.insert (transaction_id payload) processed)
                      return False

                if isDuplicate
                then do
                  status badRequest400
                  json $ object ["error" .= ("Duplicate transaction" :: T.Text)]
                else do
                  -- Validate payload
                  let validationResult = validatePayload payload

                  case validationResult of
                    Left err -> do
                      -- Send cancellation callback for validation errors
                      liftIO $ putStrLn $ "Validation failed: " ++ T.unpack err
                      callbackSuccess <- liftIO $ sendCallback cancelUrl (transaction_id payload)
                      liftIO $ putStrLn $ "Cancel callback success: " ++ show callbackSuccess
                      status badRequest400
                      json $ object ["error" .= err]
                    Right _ -> do
                      -- Send confirmation callback
                      liftIO $ putStrLn $ "Validation passed, sending confirmation callback"
                      callbackSuccess <- liftIO $ sendCallback confirmUrl (transaction_id payload)
                      liftIO $ putStrLn $ "Confirm callback success: " ++ show callbackSuccess
                      if callbackSuccess
                        then do
                          status ok200
                          json $ object ["status" .= ("success" :: T.Text)]
                        else do
                          -- Remove from processed set if callback fails
                          liftIO $ atomically $ do
                            processed <- readTVar processedTransactions
                            writeTVar processedTransactions (Set.delete (transaction_id payload) processed)
                          status internalServerError500
                          json $ object ["error" .= ("Failed to confirm transaction" :: T.Text)]

-- Validate the webhook payload
validatePayload :: WebhookPayload -> Either T.Text ()
validatePayload payload =
  -- Check for missing timestamp first
  case timestamp payload of
    Nothing -> Left "Missing timestamp"
    Just _ -> 
      -- Check for empty required fields
      if T.null (event payload)
         || T.null (transaction_id payload)
         || T.null (currency payload)
        then Left "Missing required fields"
        else
          -- Check amount validity
          let (AmountValue amt) = amount payload
          in if amt <= 0
            then Left "Invalid amount"
            else Right ()

-- Send callback to confirmation or cancellation endpoint
sendCallback :: String -> T.Text -> IO Bool
sendCallback url transId = do
  let payload = CallbackPayload { cb_transaction_id = transId }
  let requestBody = encode payload

  putStrLn $ "Sending callback to: " ++ url ++ " with transaction_id: " ++ T.unpack transId

  -- Create the request
  request <- parseRequest $ "POST " ++ url
  let request' = setRequestBodyLBS requestBody
               $ setRequestHeader "Content-Type" ["application/json"]
               $ request

  -- Send the request and handle errors
  result <- try (httpLBS request') :: IO (Either SomeException (Response BL.ByteString))

  case result of
    Left err -> do
      putStrLn $ "Callback failed to " ++ url ++ ": " ++ show err
      return False
    Right response -> do
      let statusCode = getResponseStatusCode response
      putStrLn $ "Callback sent to " ++ url ++ " with status: " ++ show statusCode
      return $ statusCode == 200