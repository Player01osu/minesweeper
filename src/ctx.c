#include "ctx.h"

static Game game_new(size_t rows, size_t cols, size_t mines)
{
	Tile **tiles = malloc(sizeof(Tile*) * (rows));
	for (size_t i = 0; i < rows; ++i) {
		tiles[i] = malloc(sizeof(Tile) * (cols));
	}

	const Game game = {
		.pan_x = 0,
		.pan_y = 0,
		.mouse_row = 0,
		.mouse_col = 0,
		.scale = 1.0,
		.state = StatePlaying,
		.tiles = tiles,
		.rows = rows,
		.cols = cols,
		.mines = mines,
		.tiles_clicked = 0,
	};
	return game;
}

void destroy_ctx(Ctx *ctx)
{
	Tile **tiles = ctx->game.tiles;
	for (size_t row = 0; row < ctx->game.rows; ++row) {
		free(tiles[row]);
	}
	free(tiles);

	for (size_t i = 0; i < 9; ++i) {
		SDL_DestroyTexture(ctx->text_ctx.num_texts[i]);
	}
	free(ctx->text_ctx.num_texts);

	SDL_DestroyRenderer(ctx->renderer);
	SDL_DestroyWindow(ctx->window);
}

Ctx ctx_new(size_t rows, size_t cols, size_t mines)
{
	SDL_Window *window = SDL_CreateWindow("TEST", SDL_WINDOWPOS_CENTERED,
					      SDL_WINDOWPOS_CENTERED, WIDTH, HEIGHT, 0);
	SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	TTF_Font *font = TTF_OpenFont("/usr/share/fonts/TTF/FiraMono-Medium.ttf", 240);
	TTF_SetFontKerning(font, 1);

	SDL_Color white = { 255, 255, 255, 255 };
	SDL_Texture **num_texts = malloc(sizeof(SDL_Texture*) * 9);

	char str[] = {'0', '\0'};
	for (size_t i = 0; i < 9; ++i) {
		*str = '0' + i;
		SDL_Surface *surface =
			TTF_RenderText_Solid(font, str, white);
		SDL_Texture *num_text =
			SDL_CreateTextureFromSurface(renderer, surface);
		num_texts[i] = num_text;
		SDL_FreeSurface(surface);
	}

	const Ctx ctx = {
		.window = window,
		.renderer = renderer,
		.text_ctx = {
			.font = font,
			.num_texts = num_texts,
		},
		.game = game_new(rows, cols, mines),
	};
	return ctx;
}
