#!/bin/bash

# Build the project
echo "Building the webhook server..."
cabal build

# Run the webhook server
echo "Starting webhook server on port 5001..."
cabal run Functional-Webhook