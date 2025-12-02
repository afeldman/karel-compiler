FROM haskell:9.4.8

# Use Debian archive for Buster (EOL)
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's|security.debian.org|archive.debian.org|g' /etc/apt/sources.list && \
    sed -i '/stretch-updates/d' /etc/apt/sources.list

# Install LLVM 15 from official repo
RUN apt-get update && apt-get install -y \
    wget \
    gnupg \
    software-properties-common \
    clang \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Add LLVM 15 repo and install
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    echo "deb http://apt.llvm.org/buster/ llvm-toolchain-buster-15 main" > /etc/apt/sources.list.d/llvm.list && \
    apt-get update && apt-get install -y \
    llvm-15 \
    llvm-15-dev \
    libllvm15 \
    && rm -rf /var/lib/apt/lists/*

# Set LLVM paths
ENV LLVM_CONFIG=/usr/lib/llvm-15/bin/llvm-config
ENV PATH="/usr/lib/llvm-15/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/lib/llvm-15/lib:${LD_LIBRARY_PATH}"

# Install Haskell tools
RUN cabal update && cabal install BNFC alex happy

# Set working directory
WORKDIR /workspace

# Copy project files
COPY . /workspace/

# Default command
CMD ["/bin/bash"]
