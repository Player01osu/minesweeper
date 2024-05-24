#ifndef MINES_H
#define MINES_H

#include <stdbool.h>
#include <SDL2/SDL.h>
#include "minesweeper.h"
#include "ctx.h"

void generate_mines(void);

void offset_mines(size_t row, size_t col);

void calculate_surround(void);

Uint8 sum_surround_flagged(const Sint32 row, const Sint32 col);

#endif /* MINES_H */
