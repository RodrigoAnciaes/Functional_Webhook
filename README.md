# Functional Webhook

A Haskell-based webhook server that processes payment events and validates transactions.

## Features

- Token-based authentication
- Payload validation
- Duplicate transaction prevention
- Asynchronous callbacks for transaction confirmation/cancellation
- RESTful API built with Scotty

## Requirements

- Stack (Haskell Tool Stack)
- Python 3.x (for running tests)

## Setup

### Using DevContainer (Recommended)

1. Open the project in VS Code
2. When prompted, click "Reopen in Container"
3. Wait for the container to build and the environment to set up

### Manual Setup

1. Install Stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/
2. Install dependencies and build:
   ```bash
   stack setup
   stack build
   ```

## Running the Webhook Server

```bash
# Make the script executable
chmod +x run_webhook.sh

# Run the server
./run_webhook.sh
```

Or directly with stack:
```bash
stack run
```

The server will start on port 5001.

## Testing

Run the Python test suite:
```bash
python test_webhook.py
```

The test suite will:
1. Start local endpoints for `/confirmar` and `/cancelar`
2. Send various webhook payloads to test different scenarios
3. Validate the responses and callbacks

## API Endpoints

### POST /webhook

Receives payment event webhooks.

**Headers:**
- `X-Webhook-Token`: Authentication token (required)
- `Content-Type`: application/json

**Request Body:**
```json
{
  "event": "payment_success",
  "transaction_id": "abc123",
  "amount": 49.90,
  "currency": "BRL",
  "timestamp": "2025-05-11T16:00:00Z"
}
```

**Validation Rules:**
- Token must match the expected value
- All fields are required
- Amount must be greater than 0
- Transaction IDs must be unique

**Response:**
- 200 OK: Transaction processed successfully
- 400 Bad Request: Invalid payload or duplicate transaction
- 401 Unauthorized: Invalid or missing token

## Architecture

The webhook server uses:
- **Scotty**: Lightweight web framework
- **Aeson**: JSON parsing and encoding
- **STM**: Software Transactional Memory for thread-safe duplicate detection
- **http-conduit**: HTTP client for callbacks

## Development

To modify the webhook logic, edit `app/Main.hs`. The main components are:

1. **Token validation**: Checks the `X-Webhook-Token` header
2. **Payload parsing**: Deserializes JSON into Haskell data types
3. **Duplicate detection**: Uses STM TVar with a Set to track processed transactions
4. **Validation**: Checks required fields and business rules
5. **Callbacks**: Sends confirmation or cancellation requests based on validation

### Building and Development Commands

```bash
# Setup project (first time)
stack setup

# Build the project
stack build

# Run the application
stack run

# Install dependencies
stack install

# Clean build artifacts
stack clean

# Run with development flags
stack build --fast --file-watch
```