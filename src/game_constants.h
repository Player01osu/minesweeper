#ifndef GAME_CONSTANTS_H
#define GAME_CONSTANTS_H

#include <SDL2/SDL.h>

#define WIDTH 1280
#define HEIGHT 720

const Uint32 BACKGROUND_COLOR = 0x888888;
const Uint32 TILE_UNCLICKED_COLOR = 0x111111;
const Uint32 TILE_CLICKED_COLOR = 0xFFFFFF;

#define COLS 10
#define ROWS 10
const Uint32 PAD_INNER = 5;
const Uint32 PAD_OUTER = 30;

#endif /* GAME_CONSTANTS_H */
