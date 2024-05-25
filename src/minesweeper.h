#ifndef MINESWEEPER_H
#define MINESWEEPER_H

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "ctx.h"

//#define WIDTH  1920
//#define HEIGHT 1080

#define WIDTH  1280
#define HEIGHT 720
#define FPS    30

/* SDL_Color format is ABGR on little-endian machines*/
#define BACKGROUND_COLOR                0xFF444444
#define TILE_UNCLICKED_COLOR            0xFF000000
#define TILE_UNCLICKED_HIGHLIGHT_COLOR  0xFF0F0F0F
#define TILE_CLICKED_COLOR              0xFFAAAAAA
#define TILE_CLICKED_MINE_COLOR         0xFF0000FF
#define TILE_FLAGGED_COLOR              0xFFAABB55
#define TILE_FLAGGED_HIGHLIGHT_COLOR    0xFFBACB65
#define TILE_NUM_COLOR                  0xFFBBA100

#define DEFAULT_ROWS   9
#define DEFAULT_COLS   9
#define DEFAULT_MINES  10
#define PAD_INNER      0
#define PAD_OUTER      30

#define for_each_tile(tile) \
	for (size_t _row = 0; _row < game.rows; ++_row) \
		for (size_t _col = 0; _col < game.cols; ++_col) \
			if ((tile = &game.tiles[_row][_col]))

#define for_each_enum_tile(row, col, tile) \
	for (row = 0; row < game.rows; ++row) \
		for (col = 0; col < game.cols; ++col) \
			if ((tile = &game.tiles[row][col]))

typedef enum {
	StatePlaying,
	StateWin,
	StateLose,
} State;

typedef enum {
	TileStateUnclicked,
	TileStateClicked,
	TileStateFlagged,
} TileState;

typedef struct {
	TTF_Font *font;
	SDL_Texture *num_texts[10];
} TextCtx;

typedef struct {
	SDL_Rect rect;
	/* 0-8 */
	Uint8 surround_mines;
	bool mine;
	TileState state;
} Tile;

typedef struct {
	float scale;
	int pan_x;
	int pan_y;
	size_t mouse_row;
	size_t mouse_col;
	State state;
	Tile **tiles;
	size_t rows;
	size_t cols;
	size_t mines;
	size_t tiles_clicked;
	SDL_Rect game_layout;
} Game;

extern SDL_Window *window;
extern SDL_Renderer *renderer;
extern TextCtx text_ctx;
extern Game game;

bool is_valid_idx(size_t row, size_t col);

#endif /* MINESWEEPER_H */
