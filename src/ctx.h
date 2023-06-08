#ifndef CTX_H
#define CTX_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include "game_constants.h"

typedef struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
	TTF_Font *font;
} Ctx;

void destroy_ctx(Ctx ctx);

Ctx ctx_new();

#endif /* CTX_H */
