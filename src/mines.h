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
} Tile;

void generate_mines(Tile tile[ROWS][COLS]);

void offset_mines(void);

#endif /* MINES_H */
