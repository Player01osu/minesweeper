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

#define WIDTH  1920
#define HEIGHT 1080
#define FPS    30

#define BACKGROUND_COLOR                0x444444
#define TILE_UNCLICKED_COLOR            0x000000
#define TILE_UNCLICKED_HIGHLIGHT_COLOR  0x0F0F0F
#define TILE_CLICKED_COLOR              0xAAAAAA
#define TILE_CLICKED_MINE_COLOR         0xFF0000
#define TILE_FLAGGED_COLOR              0x55BBAA
#define TILE_FLAGGED_HIGHLIGHT_COLOR    0x65CBBA
#define TILE_NUM_COLOR                  0x00A1BB

#define DEFAULT_ROWS   9
#define DEFAULT_COLS   9
#define DEFAULT_MINES  10
#define PAD_INNER      0
#define PAD_OUTER      30

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
	SDL_Texture **num_texts;
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

typedef struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
	TextCtx text_ctx;
	Game game;
} Ctx;

void destroy_ctx(Ctx *ctx);

Ctx ctx_new(size_t rows, size_t cols, size_t mines);

bool is_valid_idx(const Game *game, size_t row, size_t col);

#endif /* MINESWEEPER_H */
