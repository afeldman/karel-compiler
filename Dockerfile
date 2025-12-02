FROM alpine:latest

# Install C++ build tools and LLVM 15
RUN apk upgrade --no-cache && \
    apk add --no-cache \
        build-base \
        cmake \
        flex \
        llvm15 \
        llvm15-dev \
        llvm15-static \
        clang15 \
        git \
        ghc \
        cabal \
        bash \
        wget \
        m4

# Build Bison 3.8.2 from source (Alpine only has ancient 2.3)
RUN wget https://ftp.gnu.org/gnu/bison/bison-3.8.2.tar.gz && \
    tar -xzf bison-3.8.2.tar.gz && \
    cd bison-3.8.2 && \
    ./configure --prefix=/usr && \
    make -j$(nproc) && \
    make install && \
    cd .. && \
    rm -rf bison-3.8.2 bison-3.8.2.tar.gz

# Set LLVM 15 environment
ENV LLVM_DIR=/usr/lib/llvm15
ENV PATH="${LLVM_DIR}/bin:${PATH}"
ENV CC=clang-15
ENV CXX=clang++-15

# Install BNFC
RUN cabal update && \
    cabal install --install-method=copy --installdir=/usr/local/bin BNFC-2.9.6.1

WORKDIR /workspace

