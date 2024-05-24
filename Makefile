CC := clang
CFLAGS := -Wall -Wextra -Wpedantic -std=c99 -ggdb
SRCS := $(wildcard src/*.c)
OBJS := $(patsubst src/%.c, target/debug/%.o, $(SRCS))
DEPS := $(patsubst src/%.c, target/debug/%.d, $(SRCS))
LINKING := -lm -lSDL2 -lSDL2_ttf -L/usr/lib -I/usr/include/SDL2 -D_REENTRANT

EXEC := target/debug/minesweeper

#.PHONY: all clean

all: $(EXEC)

-include $(DEPS)

run: all
	$(EXEC)

$(EXEC): $(OBJS)
	$(CC) -o $@ $(OBJS) $(CFLAGS) $(LINKING)

target/debug/%.o: src/%.c src/%.h
	$(CC) -MMD -MP -c -o $@ $< $(CFLAGS)

clean:
	rm -f target/debug/*
