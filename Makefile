# Compiler and flags
GHC = ghc
GHCFLAGS = -Wall

# Binary name
TARGET = main

# Source file
SOURCE = main.hs trie.hs

# Default target
all: $(TARGET)

# Compile the program
$(TARGET): $(SOURCE)
	$(GHC) $(GHCFLAGS) -o $(TARGET) $(SOURCE)

# Run the program
run: $(TARGET)
	./$(TARGET)

# Clean build files
clean:
	rm -f $(TARGET) *.hi *.o

# Clean and rebuild
rebuild: clean all

.PHONY: all run clean rebuild test-input