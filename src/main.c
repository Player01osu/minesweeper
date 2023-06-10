#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_pixels.h>
#include <SDL2/SDL_ttf.h>

#include "game_constants.h"
#include "colors.h"
#include "ctx.h"
#include "mines.h"

#define MIN(a, b) a > b ? b : a

SDL_Rect rect_new(int x, int y, int w, int h)
{
	const SDL_Rect rect = {
		x,
		y,
		w,
		h,
	};
	return rect;
}

typedef struct {
	const Uint32 rect_width;
	const Uint32 rect_height;
	const Uint32 board_width;
	const Uint32 board_height;
} GridCalc;

bool is_valid_idx(const Game *game, size_t row, size_t col)
{
	const size_t cols = game->cols;
	const size_t rows = game->rows;

	return col >= 0 && col < cols && row >= 0 && row < rows;
}

GridCalc grid_calc(const size_t rows, const size_t cols)
{
	const Uint32 min_canvas = MIN(WIDTH, HEIGHT);

	const Uint32 width_adjusted = min_canvas - PAD_OUTER - PAD_INNER * cols;
	const Uint32 height_adjusted = min_canvas - PAD_OUTER - PAD_INNER * rows;

	const Uint32 rect_height = MIN(height_adjusted / rows, width_adjusted / cols);
	const Uint32 rect_width = MIN(height_adjusted / rows, width_adjusted / cols);

	const Uint32 board_width = cols * (rect_width + PAD_INNER);
	const Uint32 board_height = rows * (rect_height + PAD_INNER);

	const GridCalc calc = { rect_width, rect_height, board_width, board_height };
	return calc;
}

SDL_Rect grid_tile(const Game *game, size_t row, size_t col)
{
	const GridCalc calc = grid_calc(game->rows, game->cols);
	const Uint32 rect_width = calc.rect_width;
	const Uint32 rect_height = calc.rect_height;
	const Uint32 board_width = calc.board_width;
	const Uint32 board_height = calc.board_height;

	const Uint32 x = (WIDTH - board_width) / 2 + col * (rect_width + PAD_INNER);
	const Uint32 y = (HEIGHT - board_height) / 2 + row * (rect_height + PAD_INNER);

	return rect_new(x, y, rect_width, rect_height);
}

void coord_to_index(const Game *game, const Sint32 x, const Sint32 y, size_t *row, size_t *col)
{
	const GridCalc calc = grid_calc(game->rows, game->cols);
	const Uint32 rect_width = calc.rect_width;
	const Uint32 rect_height = calc.rect_height;
	const Uint32 board_width = calc.board_width;
	const Uint32 board_height = calc.board_height;

	*row = (y - ((HEIGHT - board_height) / 2)) / (rect_height + PAD_INNER);
	*col = (x - ((WIDTH - board_width) / 2)) / (rect_width + PAD_INNER);
}
void expand_cavern(Ctx *ctx, size_t row, size_t col, bool *opening);

void toggle_tile(Ctx *ctx, size_t row, size_t col, bool *opening)
{
	Tile **tiles = ctx->game.tiles;
	if (!is_valid_idx(&ctx->game, row, col))
		return;

	Tile *tile = &tiles[row][col];
	if (tile->clicked)
		return;
	if (tile->flagged) {
		tile->flagged = false;
		return;
	}

	if (*opening && tile->mine) {
		offset_mines(&ctx->game, row, col);
	}
	*opening = false;
	tile->clicked = !tile->clicked;

	if (tile->mine) {
		ctx->game.state = StateLose;
		printf("You Lost\n");
		return;
	}

	if (tile->surround_mines == 0)
		expand_cavern(ctx, row, col, opening);

	++ctx->game.tiles_clicked;
	const size_t tiles_safe = ctx->game.rows*ctx->game.cols - ctx->game.mines;
	if (ctx->game.tiles_clicked == tiles_safe) {
		ctx->game.state = StateWin;
		printf("You Won\n");
		return;
	}
}

void flag_tile(Game *game, size_t row, size_t col)
{
	if (!is_valid_idx(game, row, col))
		return;

	Tile *tile = &game->tiles[row][col];
	tile->flagged = !tile->flagged;
}

void expand_cavern(Ctx *ctx, size_t row, size_t col, bool *opening)
{
	Tile **tiles = ctx->game.tiles;
	if (!is_valid_idx(&ctx->game, row, col))
		return;
	Tile *tile = &tiles[row][col];

	if (tile->surround_mines != 0)
		return;

	toggle_tile(ctx, row + 1, col + 1, opening);
	toggle_tile(ctx, row + 1, col - 0, opening);
	toggle_tile(ctx, row + 1, col - 1, opening);

	toggle_tile(ctx, row + 0, col + 1, opening);
	toggle_tile(ctx, row + 0, col - 1, opening);

	toggle_tile(ctx, row - 1, col + 1, opening);
	toggle_tile(ctx, row - 1, col - 0, opening);
	toggle_tile(ctx, row - 1, col - 1, opening);
}

void create_grid(Game *game)
{
	for (size_t row = 0; row < game->rows; ++row) {
		for (size_t col = 0; col < game->cols; ++col) {
			Tile tile = {
				.rect = grid_tile(game, row, col),
				.clicked = false,
			};
			game->tiles[row][col] = tile;
		}
	}
	generate_mines(game);
}

void draw_grid(Ctx *ctx)
{
	Tile **tiles = ctx->game.tiles;
	const size_t rows = ctx->game.rows;
	const size_t cols = ctx->game.cols;
	for (size_t row = 0; row < rows; ++row) {
		for (size_t col = 0; col < cols; ++col) {
			Tile tile = tiles[row][col];
			if (tile.flagged) {
				set_render_color_u32(ctx, TILE_FLAGGED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
				continue;
			}

			if (tile.clicked && tile.mine) {
				set_render_color_u32(ctx, TILE_CLICKED_MINE_COLOR,
						     SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
			} else if (tile.clicked) {
				set_render_color_u32(ctx, TILE_CLICKED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);

				if (tile.surround_mines == 0)
					continue;
				SDL_Color white = { 255, 255, 255 };
				char num[] = "0";
				num[0] = tile.surround_mines + '0';

				SDL_Surface *surface =
					TTF_RenderText_Solid(ctx->text_ctx.font, num, white);
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

// TODO: Parse arguments such as rows and cols (ie: --size=10x10) and
// number of mines (ie: --mines=10)
void parse_args(int argc, char **argv)
{
	fprintf(stderr, "ERROR: parse_args(..) UNIMPLEMENTED\n");
	assert(false);
}

int main(int argc, char **argv)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		printf("error initializing SDL: %s\n", SDL_GetError());
	}
	if (TTF_Init() != 0) {
		printf("error initializing SDL: %s\n", SDL_GetError());
	}

	size_t rows = DEFAULT_ROWS;
	size_t cols = DEFAULT_COLS;
	size_t mines = DEFAULT_MINES;

	if (argc > 1) {
		parse_args(argc, argv);
	}

	Ctx ctx = ctx_new(rows, cols, mines);
	create_grid(&ctx.game);

	bool running = true;
	bool opening = true;
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
					const Sint32 x = event.button.x;
					const Sint32 y = event.button.y;
					size_t row, col;
					coord_to_index(&ctx.game, x, y, &row, &col);
					toggle_tile(&ctx, row, col, &opening);
				} break;
				case SDL_BUTTON_RIGHT: {
					const Sint32 x = event.button.x;
					const Sint32 y = event.button.y;
					size_t row, col;
					coord_to_index(&ctx.game, x, y, &row, &col);
					flag_tile(&ctx.game, row, col);
				} break;
				}
			}
		}

		draw_grid(&ctx);

		SDL_RenderPresent(ctx.renderer);
		SDL_Delay(1000 / FPS);
	}

	destroy_ctx(&ctx);
	SDL_Quit();
	TTF_Quit();
	return 0;
}
