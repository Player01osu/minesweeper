#ifndef CTX_H
#define CTX_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include "game_constants.h"

typedef struct {
	TTF_Font *font;
} TextCtx;

typedef struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
	TextCtx text_ctx;
} Ctx;

void destroy_ctx(Ctx ctx);

Ctx ctx_new();

#endif /* CTX_H */
