# Makefile for sniffert
FC = gfortran
CC = gcc
FFLAGS = -O2 -Wall -ffree-line-length-none
CFLAGS = -O2 -Wall
LDFLAGS = -lncurses
SRC_DIR = src
APP_DIR = app
BUILD_DIR = build
BIN_DIR = .

# C helper files
C_SOURCES = dir_helpers.c ncurses_helpers.c
C_OBJS = $(C_SOURCES:%.c=$(BUILD_DIR)/%.o)

# Fortran module files (order matters for dependencies)
MODULES = types.f90 file_system.f90 ncurses_wrapper.f90 disk_scanner.f90 \
          treemap_layout.f90 navigation.f90 terminal_ui.f90
MODULE_OBJS = $(MODULES:%.f90=$(BUILD_DIR)/%.o)

# Main program
MAIN = main.f90
MAIN_OBJ = $(BUILD_DIR)/main.o

# Target executable
TARGET = $(BIN_DIR)/sniffert

# Disable parallel builds to ensure proper module compilation order
.NOTPARALLEL:

.PHONY: all clean

all: $(TARGET)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Compile C helper files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Build modules with explicit dependencies
$(BUILD_DIR)/types.o: $(SRC_DIR)/types.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/file_system.o: $(SRC_DIR)/file_system.f90 $(BUILD_DIR)/types.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/ncurses_wrapper.o: $(SRC_DIR)/ncurses_wrapper.f90 $(BUILD_DIR)/types.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/disk_scanner.o: $(SRC_DIR)/disk_scanner.f90 $(BUILD_DIR)/file_system.o $(BUILD_DIR)/types.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/treemap_layout.o: $(SRC_DIR)/treemap_layout.f90 $(BUILD_DIR)/types.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/navigation.o: $(SRC_DIR)/navigation.f90 $(BUILD_DIR)/types.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/terminal_ui.o: $(SRC_DIR)/terminal_ui.f90 $(BUILD_DIR)/types.o $(BUILD_DIR)/ncurses_wrapper.o $(BUILD_DIR)/treemap_layout.o $(BUILD_DIR)/navigation.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Build main program
$(MAIN_OBJ): $(APP_DIR)/$(MAIN) $(MODULE_OBJS) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Link everything
$(TARGET): $(C_OBJS) $(MODULE_OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -rf $(BUILD_DIR) $(TARGET)
