module Main where

import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson
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

-- Data types for the webhook payload
data WebhookPayload = WebhookPayload
  { event :: T.Text
  , transaction_id :: T.Text
  , amount :: Double
  , currency :: T.Text
  , timestamp :: Maybe T.Text  -- Optional to handle missing field test
  } deriving (Show, Generic)

instance FromJSON WebhookPayload
instance ToJSON WebhookPayload

-- Data type for callback payloads
data CallbackPayload = CallbackPayload
  { transaction_id :: T.Text
  } deriving (Show, Generic)

instance ToJSON CallbackPayload

-- Expected token for authentication
expectedToken :: T.Text
expectedToken = "meu-token-secreto"

-- Callback URLs
confirmUrl :: String
confirmUrl = "http://127.0.0.1:5001/confirmar"

cancelUrl :: String
cancelUrl = "http://127.0.0.1:5001/cancelar"

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
                -- Check for duplicate transaction
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
                      -- Send cancellation callback
                      _ <- liftIO $ sendCallback cancelUrl (transaction_id payload)
                      status badRequest400
                      json $ object ["error" .= err]
                    Right _ -> do
                      -- Send confirmation callback
                      success <- liftIO $ sendCallback confirmUrl (transaction_id payload)
                      if success
                        then do
                          status ok200
                          json $ object ["status" .= ("success" :: T.Text)]
                        else do
                          status internalServerError500
                          json $ object ["error" .= ("Failed to confirm transaction" :: T.Text)]

-- Validate the webhook payload
validatePayload :: WebhookPayload -> Either T.Text ()
validatePayload payload = do
  -- Check if timestamp is present
  case timestamp payload of
    Nothing -> Left "Missing timestamp"
    Just _ -> Right ()
  
  -- Check if amount is valid (greater than 0)
  if amount payload <= 0
    then Left "Invalid amount"
    else Right ()
  
  -- Check if required fields are not empty
  if T.null (event payload) || T.null (transaction_id payload) || T.null (currency payload)
    then Left "Missing required fields"
    else Right ()

-- Send callback to confirmation or cancellation endpoint
sendCallback :: String -> T.Text -> IO Bool
sendCallback url transId = do
  let payload = CallbackPayload { transaction_id = transId }
  let requestBody = encode payload
  
  -- Create the request
  request <- parseRequest $ "POST " ++ url
  let request' = setRequestBodyLBS requestBody
               $ setRequestHeader "Content-Type" ["application/json"]
               $ request
  
  -- Send the request and handle errors
  result <- try (httpLBS request') :: IO (Either SomeException (Response BL.ByteString))
  
  case result of
    Left _ -> return False
    Right response -> return $ getResponseStatusCode response == 200