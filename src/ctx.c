#include "ctx.h"

void destroy_ctx(Ctx ctx)
{
	SDL_DestroyRenderer(ctx.renderer);
	SDL_DestroyWindow(ctx.window);
}

Ctx ctx_new()
{
	SDL_Window *window = SDL_CreateWindow("TEST", SDL_WINDOWPOS_CENTERED,
					      SDL_WINDOWPOS_CENTERED, WIDTH, HEIGHT, 0);
	SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	TTF_Font *font = TTF_OpenFont("/usr/share/fonts/TTF/FiraMono-Medium.ttf", 240);
	TTF_SetFontKerning(font, 1);

	State state = {
		.lose = false,
	};

	Ctx ctx = {
		window,
		renderer,
		font,
		state,
	};
	return ctx;
}
