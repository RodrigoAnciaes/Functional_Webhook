#!/bin/bash
echo "Setting up environment..."

# Initialize opam
opam init -y --disable-sandboxing || true
eval $(opam env)

# Update Cabal and GHCup paths
echo 'export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH"' >> ~/.bashrc
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH"

# Update cabal package index
echo "Updating Cabal package index..."
cabal update

# Install Haskell tools
echo "Installing Haskell tools..."
cabal install --install-method=copy --overwrite-policy=always hlint stylish-haskell || true

# Build the webhook project if we're in the right directory
if [ -f "Functional-Webhook.cabal" ]; then
    echo "Building the webhook project..."
    cabal build
else
    echo "Cabal file not found in current directory. Please build manually with 'cabal build' from the project root."
fi

echo "Environment setup complete!"