# Karel Runtime Library Makefile

CC = clang
CFLAGS = -Wall -Wextra -O2 -fPIC
AR = ar
ARFLAGS = rcs

# Targets
LIB = libkarel.a
OBJ = runtime.o

# Default target
all: $(LIB)

# Build static library
$(LIB): $(OBJ)
	$(AR) $(ARFLAGS) $@ $^
	@echo "Built $(LIB)"

# Compile runtime
runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -c runtime.c -o runtime.o

# Clean
clean:
	rm -f $(OBJ) $(LIB)

# Install (optional)
install: $(LIB)
	@mkdir -p ../lib
	@cp $(LIB) ../lib/
	@cp runtime.h ../lib/
	@echo "Installed to ../lib/"

.PHONY: all clean install
