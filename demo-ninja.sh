#!/bin/bash
# Demo script for Karel Ninja build system

echo "=== Karel Ninja Build System Demo ==="
echo ""

# Check if ninja is installed
if ! command -v ninja &> /dev/null; then
    echo "❌ Ninja not installed!"
    echo "Install with: brew install ninja (macOS) or apt-get install ninja-build (Linux)"
    exit 1
fi

echo "✓ Ninja found: $(ninja --version)"
echo ""

# Step 1: Configure
echo "Step 1: Configuring build system..."
./configure.py
echo ""

# Step 2: Show targets
echo "Step 2: Available targets:"
ninja -t targets all | head -20
echo "... (more targets available)"
echo ""

# Step 3: Build all
echo "Step 3: Building all targets..."
echo "(Note: This will fail if karelc is not built yet)"
ninja -n all  # Dry-run to show what would be built
echo ""

# Step 4: Show LLVM targets
echo "Step 4: LLVM IR targets:"
ninja -t targets llvm
echo ""

# Step 5: Show preprocessor targets
echo "Step 5: Preprocessor targets:"
ninja -t targets preprocess
echo ""

# Step 6: Debug configuration
echo "Step 6: Debug configuration example:"
./configure.py --debug --output debug.ninja
echo ""
cat debug.ninja | head -15
echo "..."
echo ""

# Step 7: Release configuration
echo "Step 7: Release configuration example:"
./configure.py --release --output release.ninja
echo ""

echo "=== Demo Complete ==="
echo ""
echo "Next steps:"
echo "  1. Build Karel compiler: cd .. && task build:karel"
echo "  2. Run: ninja"
echo "  3. Check output: ls -la build/"
echo ""
echo "Documentation:"
echo "  - README.md - General usage"
echo "  - NINJA.md - Ninja build system"
echo "  - PREPROCESSOR.md - Preprocessor features"
