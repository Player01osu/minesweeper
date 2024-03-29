#ifndef CTX_H
#define CTX_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>
#include "game_constants.h"

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
} Game;

typedef struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
	TextCtx text_ctx;
	Game game;
} Ctx;

void destroy_ctx(Ctx *ctx);

Ctx ctx_new(size_t rows, size_t cols, size_t mines);

#endif /* CTX_H */
