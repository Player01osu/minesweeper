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

Tile tiles[ROWS][COLS];

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
	Uint32 board_width;
	Uint32 board_height;
} GridCalc;

#define MIN(a, b) a > b ? b : a

GridCalc grid_calc(void)
{
	Uint32 min_canvas = MIN(WIDTH, HEIGHT);

	Uint32 width_adjusted = min_canvas - PAD_OUTER - PAD_INNER * COLS;
	Uint32 height_adjusted = min_canvas - PAD_OUTER - PAD_INNER * ROWS;

	Uint32 rect_height = MIN(height_adjusted / ROWS, width_adjusted / COLS);
	Uint32 rect_width = MIN(height_adjusted / ROWS, width_adjusted / COLS);

	Uint32 board_width = COLS * (rect_width + PAD_INNER);
	Uint32 board_height = ROWS * (rect_height + PAD_INNER);

	GridCalc calc = { rect_width, rect_height, board_width, board_height};
	return calc;
}

SDL_Rect grid_tile(size_t row, size_t col)
{
	GridCalc calc = grid_calc();
	Uint32 rect_width = calc.rect_width;
	Uint32 rect_height = calc.rect_height;
	Uint32 board_width = calc.board_width;
	Uint32 board_height = calc.board_height;

	Uint32 x = (WIDTH - board_width) / 2 + col * (rect_width + PAD_INNER);
	Uint32 y = (HEIGHT - board_height) / 2 + row * (rect_height + PAD_INNER);

	return rect_new(x, y, rect_width, rect_height);
}

void toggle_tile(Ctx *ctx, Uint32 x, Uint32 y, bool *opening)
{
	GridCalc calc = grid_calc();
	Uint32 rect_width = calc.rect_width;
	Uint32 rect_height = calc.rect_height;
	Uint32 board_width = calc.board_width;
	Uint32 board_height = calc.board_height;

	Uint32 col = (x - ((WIDTH - board_width) / 2)) / (rect_width + PAD_INNER);
	Uint32 row = (y - ((HEIGHT - board_height) / 2)) /  (rect_height + PAD_INNER);

	if (col < 0 || col > COLS - 1 || row < 0 || row > ROWS - 1)
		return;

	Tile *tile = &tiles[row][col];
	if (*opening && tile->mine) {
		offset_mines(ctx, tiles, row, col);
		calculate_surround(tiles, ctx->offset);
		printf("%d\n", ctx->offset);
	}
	*opening = false;
	tile->clicked = !tile->clicked;
}

void create_grid(Ctx ctx)
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			Tile tile = {
				.rect = grid_tile(row, col),
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
				set_render_color_u32(ctx, TILE_CLICKED_MINE_COLOR,
						     SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
			} else if (tile.clicked) {
				set_render_color_u32(ctx, TILE_CLICKED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);

				//set_render_color_u32(ctx, TILE_NUM_COLOR, SDL_ALPHA_OPAQUE);
				SDL_Color white = { 255, 255, 255 };
				char num[] = "0";
				num[0] = tile.surround_mines + '0';

				SDL_Surface *surface = TTF_RenderText_Solid(ctx->text_ctx.font, num, white);
				SDL_Texture *num_text =
					SDL_CreateTextureFromSurface(ctx->renderer, surface);

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

void clear_background(Ctx *ctx)
{
	set_render_color_u32(ctx, BACKGROUND_COLOR, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(ctx->renderer);
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
					toggle_tile(&ctx, x, y, &opening);
				} break;
				}
			}
		}

		draw_grid(&ctx);

		SDL_RenderPresent(ctx.renderer);
		SDL_Delay(1000 / FPS);
	}

	destroy_ctx(ctx);
	SDL_Quit();
	TTF_Quit();
	return 0;
}
