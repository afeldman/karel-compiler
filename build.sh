#!/bin/bash

set -e

echo "Building Karel Compiler (C backend version)..."
echo "Note: This requires Docker with Bison 3.8.2"
echo ""

# Check if running in Docker
if [ -f /.dockerenv ]; then
    echo "Running inside Docker container..."
    IN_DOCKER=1
else
    echo "Running on host, will use docker-compose..."
    IN_DOCKER=0
fi

if [ $IN_DOCKER -eq 0 ]; then
    # Run build inside Docker container
    docker-compose run --rm karel-builder bash -c 'bash build.sh'
    exit $?
fi

# Build runtime library
echo "Building runtime library..."
cd runtime
make clean
make
cd ..

# Generate C parser with BNFC
echo "Generating C parser..."
rm -rf parser-c
bnfc --c -m -o parser-c karel.cf

# Build parser library
echo "Building parser..."
cd parser-c
make clean || true
make
cd ..

# Create build directory
echo "Configuring CMake..."
rm -rf build
mkdir -p build
cd build

# Configure and build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc || echo 4)

echo ""
echo "✓ Build complete! Executable: build/karelc"
echo ""
echo "Note: The binary runs on Linux. On macOS/Windows, use the wrapper:"
echo "  ./karelc --help"
echo "  ./karelc --emit-c test_minimal.kl"
echo ""
echo "Or run directly in Docker:"
echo "  docker-compose run --rm karel-builder ./build/karelc --help"
