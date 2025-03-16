# Compiler and flags
GHC = ghc
GHCFLAGS = -Wall

# Binary name
TARGET = main

# Source file
SOURCE = Main.hs Trie.hs Regex.hs

build: $(TARGET)

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