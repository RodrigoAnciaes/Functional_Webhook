# Base image with VS Code setup
FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libtinfo-dev \
    libsystemd-dev \
    libgmp-dev \
    libnuma-dev \
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

USER vscode

# Install GHCup for Haskell
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin
ENV PATH=${PATH}:/home/vscode/.cabal/bin

# Install Haskell tools
RUN bash -c "source ~/.ghcup/env && ghcup upgrade" && \
    bash -c "source ~/.ghcup/env && ghcup install cabal latest" && \
    bash -c "source ~/.ghcup/env && ghcup install ghc latest" && \
    bash -c "source ~/.ghcup/env && ghcup install stack latest" && \
    bash -c "source ~/.ghcup/env && ghcup set ghc latest" && \
    bash -c "source ~/.ghcup/env && ghcup set cabal latest" && \
    bash -c "source ~/.ghcup/env && ghcup set stack latest"

# Install Python packages for the webhook testing
RUN python3 -m pip install --user --upgrade pip && \
    python3 -m pip install --user fastapi uvicorn requests asyncio

# Set working directory
WORKDIR /workspaces/Functional_Webhook

# Ensure Haskell environment is loaded
RUN echo 'source ~/.ghcup/env' >> ~/.bashrc