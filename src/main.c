#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_pixels.h>
#include <SDL2/SDL_ttf.h>

#include "game_constants.h"
#include "colors.h"
#include "ctx.h"
#include "mines.h"

Tile tiles[COLS][ROWS];

SDL_Rect rect_new(int x, int y, int w, int h)
{
	SDL_Rect rect = {
		x,
		y,
		w,
		h,
	};
	return rect;
}

typedef struct {
	Uint32 rect_width;
	Uint32 rect_height;
} GridCalc;

GridCalc grid_calc(void)
{
	Uint32 min_canvas = WIDTH > HEIGHT ? HEIGHT : WIDTH;

	Uint32 width_adjusted = min_canvas - PAD_OUTER - PAD_INNER * COLS;
	Uint32 height_adjusted = min_canvas - PAD_OUTER - PAD_INNER * ROWS;

	Uint32 rect_width = width_adjusted / COLS;
	Uint32 rect_height = height_adjusted / ROWS;

	GridCalc calc = {
		rect_width,
		rect_height
	};
	return calc;
}

SDL_Rect grid_rect(size_t row, size_t col)
{
	GridCalc calc = grid_calc();
	Uint32 rect_width = calc.rect_width;
	Uint32 rect_height = calc.rect_height;

	Uint32 x = (WIDTH / 2 - PAD_OUTER) / 2 + col * (rect_width + PAD_INNER);
	Uint32 y = (PAD_OUTER) / 2 + row * (rect_height + PAD_INNER);

	return rect_new(x, y, rect_width, rect_height);
}

void create_grid(Ctx ctx)
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			Tile tile = {
				.rect = grid_rect(row, col),
				.clicked = false,
			};
			tiles[row][col] = tile;
		}
	}
	generate_mines(tiles);
}

void draw_grid(Ctx *ctx)
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			Tile tile = tiles[row][col];
			if (tile.clicked && tile.mine) {
				set_render_color_u32(ctx, TILE_CLICKED_MINE_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
			} else if (tile.clicked) {
				set_render_color_u32(ctx, TILE_CLICKED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);

				//set_render_color_u32(ctx, TILE_NUM_COLOR, SDL_ALPHA_OPAQUE);
				SDL_Color white = {255, 255, 255};
				char num[] = "0";
				num[0] = tile.surround_mines + '0';

				SDL_Surface *surface = TTF_RenderText_Solid(ctx->font, num, white);
				SDL_Texture *num_text = SDL_CreateTextureFromSurface(ctx->renderer, surface);

				SDL_RenderCopy(ctx->renderer, num_text, NULL, &tile.rect);

				SDL_FreeSurface(surface);
				SDL_DestroyTexture(num_text);
			} else {
				set_render_color_u32(ctx, TILE_UNCLICKED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
			}
		}
	}
}

void toggle_tile(Uint32 x, Uint32 y)
{
	GridCalc calc = grid_calc();
	Uint32 rect_width = calc.rect_width;
	Uint32 rect_height = calc.rect_height;
	size_t col = (x - (WIDTH / 2 - PAD_OUTER) / 2) / (rect_width + PAD_INNER);
	size_t row = (y - (PAD_OUTER) / 2) / (rect_height + PAD_INNER);

	if (col < 0 || col > COLS - 1 || row < 0 || row > ROWS - 1)
		return;

	Tile *tile = &tiles[row][col];
	tile->clicked = !tile->clicked;
}

void clear_background(Ctx *ctx)
{
	set_render_color_u32(ctx, BACKGROUND_COLOR, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(ctx->renderer);
}

void capframerate(long *then, float *remainder)
{
	long wait, frameTime;

	wait = 16 + *remainder;
	*remainder -= (int)*remainder;
	frameTime = SDL_GetTicks64() - *then;
	wait -= frameTime;

	if (wait < 1) {
		wait = 1;
	}

	SDL_Delay(wait);
	*remainder += 0.667;
	*then = SDL_GetTicks64();
}

int main(int argc, char **argv)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		printf("error initializing SDL: %s\n", SDL_GetError());
	}
	if (TTF_Init() != 0) {
		printf("error initializing SDL: %s\n", SDL_GetError());
	}
	/* control framerate */
	long then;
	float remainder;

	Ctx ctx = ctx_new();
	create_grid(ctx);

	bool running = true;
	while (running) {
		clear_background(&ctx);
		SDL_Event event;
		while (SDL_PollEvent(&event)) {
			switch (event.type) {
			case SDL_KEYDOWN:
				switch (event.key.keysym.sym) {
				case SDLK_q:
					running = false;
					break;
				}
				break;
			case SDL_QUIT:
				running = false;
				break;
			case SDL_MOUSEBUTTONDOWN:
				switch (event.button.button) {
				case SDL_BUTTON_LEFT: {
					Uint32 x = event.button.x;
					Uint32 y = event.button.y;
					toggle_tile(x, y);
				} break;
				}
			}
		}

		draw_grid(&ctx);

		SDL_RenderPresent(ctx.renderer);
		capframerate(&then, &remainder);
	}

	destroy_ctx(ctx);
	SDL_Quit();
	return 0;
}
