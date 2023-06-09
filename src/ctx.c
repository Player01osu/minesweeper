#include "ctx.h"

static Game game_new(size_t rows, size_t cols, size_t mines)
{
	Game game = {
		.state = StatePlaying,
		rows,
		cols,
		mines,
		.tiles_clicked = 0,
	};
	return game;
}

void destroy_ctx(Ctx *ctx)
{
	SDL_DestroyRenderer(ctx->renderer);
	SDL_DestroyWindow(ctx->window);
}

// TODO: Pass in rows, cols, and number through environment variable.
Ctx ctx_new(size_t rows, size_t cols, size_t mines)
{
	SDL_Window *window = SDL_CreateWindow("TEST", SDL_WINDOWPOS_CENTERED,
					      SDL_WINDOWPOS_CENTERED, WIDTH, HEIGHT, 0);
	SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	TTF_Font *font = TTF_OpenFont("/usr/share/fonts/TTF/FiraMono-Medium.ttf", 240);
	TTF_SetFontKerning(font, 1);

	Ctx ctx = {
		window,
		renderer,
		font,
		.game = game_new(rows, cols, mines),
	};
	return ctx;
}
