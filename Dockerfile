FROM haskell:8

WORKDIR /opt/ffp

# Add and Install Application Code
COPY . /opt/ffp

# Set system GHC for stack
RUN stack setup

# Install Yesod
RUN stack install yesod-bin --install-ghc

# Clean
RUN stack clean --full

# Build Application
RUN stack build

EXPOSE 3000