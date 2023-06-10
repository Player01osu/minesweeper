#ifndef MINES_H
#define MINES_H

#include <stdbool.h>
#include <SDL2/SDL.h>
#include "game_constants.h"
#include "ctx.h"

void generate_mines(Game *game);

void offset_mines(Game *game, size_t row, size_t col);

void calculate_surround(Game *game);

#endif /* MINES_H */
