#!/bin/bash
echo "Setting up Haskell and Python environment..."

# Source GHCup environment
source ~/.ghcup/env

# Update Cabal and ensure paths are in bashrc
echo 'source ~/.ghcup/env' >> ~/.bashrc
echo 'export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH"' >> ~/.bashrc

# Update cabal package index
echo "Updating Cabal package index..."
cabal update

# Install additional Haskell development tools
echo "Installing Haskell development tools..."
cabal install --install-method=copy --overwrite-policy=always \
    hlint \
    stylish-haskell \
    haskell-language-server || echo "Some tools failed to install, continuing..."

# Upgrade Python pip and install required packages
echo "Setting up Python environment..."
python3 -m pip install --user --upgrade pip
python3 -m pip install --user \
    fastapi \
    uvicorn \
    requests \
    black \
    pylint

# Build the webhook project if we're in the right directory
if [ -f "Functional-Webhook.cabal" ]; then
    echo "Building the webhook project with Cabal..."
    cabal build || echo "Cabal build failed, trying Stack..."
fi

if [ -f "stack.yaml" ]; then
    echo "Building the webhook project with Stack..."
    stack setup
    stack build || echo "Stack build failed, please build manually"
fi

# Make run script executable
if [ -f "run_webhook.sh" ]; then
    chmod +x run_webhook.sh
fi

echo "Environment setup complete!"
echo "You can now:"
echo "  - Build with: cabal build  OR  stack build"
echo "  - Run with: ./run_webhook.sh  OR  stack run"
echo "  - Test with: python3 test_webhook.py"