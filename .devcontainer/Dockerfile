# Base image with VS Code setup
FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-22.04

# Install dependencies for OCaml
RUN apt-get update && apt-get install -y \
    opam \
    m4 \
    pkg-config \
    git \
    curl \
    build-essential && \
    opam init -y && \
    eval $(opam env) && \
    opam switch create 4.14.0 && \
    opam install -y dune utop ocaml-lsp-server

USER vscode

# Install GHCup for Haskell
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal"
#RUN bash -c "ghcup set cabal 3.4.0.0"

# Install GHC
RUN bash -c "ghcup install ghc"
#RUN bash -c "ghcup set ghc 8.10.4"


#  RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y && \
#      ~/.ghcup/bin/ghcup install ghc && \
#      ~/.ghcup/bin/ghcup install cabal && \
#      ~/.ghcup/bin/ghcup install stack && \
     #~/.ghcup/bin/ghcup set ghc default

     
#     cabal update && \
#    cabal install hlint stylish-haskell

# Add paths for GHCup and opam
#ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:/root/.local/bin:$PATH"
#RUN ls -l ./
COPY postCreate.sh /home/vscode/postCreate.sh
USER root
RUN chmod +x /home/vscode/postCreate.sh