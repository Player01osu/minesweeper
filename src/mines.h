#ifndef MINES_H
#define MINES_H

#include <stdbool.h>
#include <SDL2/SDL.h>
#include "game_constants.h"

typedef struct {
	SDL_Rect rect;
	/* 0-8 */
	Uint8 surround_mines;
	bool mine;
	bool clicked;
	bool flagged;
} Tile;

void generate_mines(Tile tiles[ROWS][COLS]);

void offset_mines(Tile tiles[ROWS][COLS], size_t row, size_t col);

void calculate_surround(Tile tiles[ROWS][COLS]);

#endif /* MINES_H */
