#!/bin/bash
echo "Setting up environment..."

# Initialize opam
opam init -y
opam switch create 4.14.0
eval $(opam env)

# Update Cabal and GHCup paths
echo 'export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

cabal update
cabal install hlint stylish-haskell
