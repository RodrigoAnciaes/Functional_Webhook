# Base image with VS Code setup
FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-22.04

# Install dependencies for OCaml and Haskell
RUN apt-get update && apt-get install -y \
    opam \
    m4 \
    pkg-config \
    git \
    curl \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libtinfo-dev \
    libsystemd-dev \
    libgmp-dev && \
    opam init -y && \
    eval $(opam env) && \
    opam switch create 4.14.0 && \
    opam install -y dune utop ocaml-lsp-server

USER vscode

# Install GHCup for Haskell
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin

# Install cabal and GHC
RUN bash -c "ghcup upgrade" && \
    bash -c "ghcup install cabal" && \
    bash -c "ghcup install ghc"

# Set working directory
WORKDIR /workspaces/Functional_Webhook