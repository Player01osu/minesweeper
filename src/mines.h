#ifndef MINES_H
#define MINES_H

#include <stdbool.h>
#include <SDL2/SDL.h>
#include "minesweeper.h"
#include "ctx.h"

void generate_mines(Game *game);

void offset_mines(Game *game, size_t row, size_t col);

void calculate_surround(Game *game);

Uint8 sum_surround_flagged(Game *game, const Sint32 row, const Sint32 col);

#endif /* MINES_H */
