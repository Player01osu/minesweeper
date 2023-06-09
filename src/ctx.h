#ifndef CTX_H
#define CTX_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>
#include "game_constants.h"

typedef struct {
	TTF_Font *font;
} TextCtx;

typedef enum {
	StatePlaying,
	StateWin,
	StateLose,
} State;

typedef struct {
	State state;
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
