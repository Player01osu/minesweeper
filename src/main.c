#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>

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

#define unimplemented fprintf(stderr, "%s:%d:ERROR: %s(..) UNIMPLEMENTED\n", __FILE__, __LINE__, __func__);\
	exit(1);

static void toggle_tile(Ctx *ctx, const size_t row, const size_t col);

static void expand_cavern(Ctx *ctx, const size_t row, const size_t col);

static SDL_Rect rect_new(int x, int y, int w, int h)
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
	size_t rows;
	size_t cols;
	size_t mines;
} GameMeta;


typedef struct {
	const Uint32 rect_width;
	const Uint32 rect_height;
	const Uint32 board_width;
	const Uint32 board_height;
} GridCalc;

typedef enum {
	ParseStateSize,
	ParseStateMines,
	ParseStateNone,
} ParseState;

bool is_valid_idx(const Game *game, size_t row, size_t col)
{
	const size_t cols = game->cols;
	const size_t rows = game->rows;

	return col >= 0 && col < cols && row >= 0 && row < rows;
}

static GridCalc grid_calc(const size_t rows, const size_t cols)
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

static SDL_Rect grid_tile(const Game *game, size_t row, size_t col)
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

static void coord_to_index(const Game *game, const Sint32 x, const Sint32 y, size_t *row, size_t *col)
{
	const GridCalc calc = grid_calc(game->rows, game->cols);
	const Uint32 rect_width = calc.rect_width;
	const Uint32 rect_height = calc.rect_height;
	const Uint32 board_width = calc.board_width;
	const Uint32 board_height = calc.board_height;

	*row = ((size_t)((y + game->pan_y) / game->scale) - ((HEIGHT - board_height) / 2)) / (rect_height + PAD_INNER);
	*col = ((size_t)((x + game->pan_x) / game->scale) - ((WIDTH - board_width) / 2)) / (rect_width + PAD_INNER);
}

static void lose_game(Ctx *ctx)
{
	Tile **tiles = ctx->game.tiles;
	ctx->game.state = StateLose;

	for (size_t row = 0; row < ctx->game.rows; ++row) {
		for (size_t col = 0; col < ctx->game.cols; ++col) {
			Tile *tile = &tiles[row][col];
			if (tile->mine) tile->clicked = true;
		}
	}
	printf("You Lost\n");
}

Tile *tile_at(Ctx *ctx, const size_t row, const size_t col)
{
	Tile **tiles = ctx->game.tiles;
	if (!is_valid_idx(&ctx->game, row, col)) return NULL;
	return &tiles[row][col];
}

static void click_tile(Ctx *ctx, const size_t row, const size_t col, bool *opening)
{
	Tile *tile = tile_at(ctx, row, col);
	if (tile == NULL) return;

	if (tile->clicked) {
		if (sum_surround_flagged(&ctx->game, row, col) == tile->surround_mines) {
			expand_cavern(ctx, row, col);
		}
		return;
	}

	if (tile->flagged) {
		tile->flagged = false;
		return;
	}

	if (*opening && tile->mine) {
		offset_mines(&ctx->game, row, col);
	}
	*opening = false;

	toggle_tile(ctx, row, col);
}

static void toggle_tile(Ctx *ctx, const size_t row, const size_t col)
{
	Tile *tile = tile_at(ctx, row, col);
	if (tile == NULL) return;

	if (tile->clicked || tile->flagged) return;

	tile->clicked = !tile->clicked;

	if (tile->mine) {
		lose_game(ctx);
		return;
	}

	if (tile->surround_mines == 0) expand_cavern(ctx, row, col);

	++ctx->game.tiles_clicked;
	const size_t tiles_safe = ctx->game.rows*ctx->game.cols - ctx->game.mines;
	if (ctx->game.tiles_clicked == tiles_safe) {
		ctx->game.state = StateWin;
		printf("You Won\n");
		return;
	}
}

static void flag_tile(Game *game, size_t row, size_t col)
{
	if (!is_valid_idx(game, row, col)) return;

	Tile *tile = &game->tiles[row][col];

	if (tile->clicked) return;

	tile->flagged = !tile->flagged;
}

static void expand_cavern(Ctx *ctx, const size_t row, const size_t col)
{
	Tile **tiles = ctx->game.tiles;
	if (!is_valid_idx(&ctx->game, row, col)) return;
	Tile *tile = &tiles[row][col];

	toggle_tile(ctx, row + 1, col + 1);
	toggle_tile(ctx, row + 1, col - 0);
	toggle_tile(ctx, row + 1, col - 1);

	toggle_tile(ctx, row + 0, col + 1);
	toggle_tile(ctx, row + 0, col - 1);

	toggle_tile(ctx, row - 1, col + 1);
	toggle_tile(ctx, row - 1, col - 0);
	toggle_tile(ctx, row - 1, col - 1);
}

static void create_grid(Game *game, bool *opening)
{
	*opening = false;
	game->state = StatePlaying;
	game->tiles_clicked = 0;
	for (size_t row = 0; row < game->rows; ++row) {
		for (size_t col = 0; col < game->cols; ++col) {
			const Tile tile = {
				.rect = grid_tile(game, row, col),
				.clicked = false,
				.flagged = false,
				.mine = false,
				.surround_mines = 0,
			};
			game->tiles[row][col] = tile;
		}
	}
	generate_mines(game);
}
static void draw_grid(Ctx *ctx)
{
	Tile **tiles = ctx->game.tiles;
	const size_t rows = ctx->game.rows;
	const size_t cols = ctx->game.cols;
	for (size_t row = 0; row < rows; ++row) {
		for (size_t col = 0; col < cols; ++col) {
			Tile tile = tiles[row][col];

			tile.rect.w = tile.rect.w * ctx->game.scale;
			tile.rect.h = tile.rect.h * ctx->game.scale;
			tile.rect.x = tile.rect.x * ctx->game.scale - ctx->game.pan_x;
			tile.rect.y = tile.rect.y * ctx->game.scale - ctx->game.pan_y;

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

				if (tile.surround_mines == 0) continue;

				SDL_RenderCopy(ctx->renderer, ctx->text_ctx.num_texts[tile.surround_mines], NULL, &tile.rect);
			} else {
				set_render_color_u32(ctx, TILE_UNCLICKED_COLOR, SDL_ALPHA_OPAQUE);
				SDL_RenderFillRect(ctx->renderer, &tile.rect);
			}
		}
	}
}

static void clear_background(Ctx *ctx)
{
	set_render_color_u32(ctx, BACKGROUND_COLOR, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(ctx->renderer);
}

const char *help = "\n\
--size [rows]x[cols] \n\
--mines [number_of_mines] \n\
\n";

static void parse_args_state(char *program, char *s, ParseState parse_state, GameMeta *meta)
{
	if (strlen(s) > 31) {
		fprintf(stderr, "%s: argument size too large '%s'\n", program , s);
		exit(1);
	}
	char *p = s;
	switch (parse_state) {
		case ParseStateSize: {
			char s_row[32] = {0};
			char s_col[32] = {0};
			char *ps_row = s_row;
			char *ps_col = s_col;

			while (*p >= '0' && *p <= '9') {
				*ps_row = *p;
				++ps_row;
				++p;
			}
			if (*p != 'x') {
				fprintf(stderr, "%s: Invalid size format '%s'\n", program, s);
				fprintf(stderr, "%s", help);
				exit(1);
			}
			++p;
			while (*p >= '0' && *p <= '9') {
				*ps_col = *p;
				++ps_col;
				++p;
			}
			if (*p != '\0') {
				fprintf(stderr, "%s: Invalid size format '%s'\n", program, s);
				fprintf(stderr, "%s", help);
				exit(1);
			}
			meta->rows = atoi(s_row);
			meta->cols = atoi(s_col);
		} break;
		case ParseStateMines: {
			char s_mines[32] = {0};
			char *ps_mines = s_mines;
			while (*p >= '0' && *p <= '9') {
				*ps_mines = *p;
				++ps_mines;
				++p;
			}
			if (*p != '\0') {
				fprintf(stderr, "%s: Invalid size format '%s'\n", program, s);
				fprintf(stderr, "%s", help);
				fprintf(stderr, "%c\n", *p);
				exit(1);
			}
			meta->mines = atoi(s_mines);
		} break;
		case ParseStateNone: {
			fprintf(stderr, "%s: Ambiguous command '%s'\n", program, s);
			fprintf(stderr, "%s", help);
			exit(1);
		} break;
	}
}

static void parse_args(int argc, char **argv, GameMeta *meta)
{
	ParseState parse_state = ParseStateNone;

	for (size_t i = 1; i < (size_t)argc; ++i) {
		if (strncmp(argv[i], "--mines", 16) == 0) {
			parse_state = ParseStateMines;
		} else if (strncmp(argv[i], "--size", 16) == 0) {
			parse_state = ParseStateSize;
		} else if (argv[i][0] == '-') {
			fprintf(stderr, "%s: unrecognized option '%s'\n", argv[0], argv[i]);
			fprintf(stderr, "%s", help);
			exit(1);
		} else {
			parse_args_state(argv[0], argv[i], parse_state, meta);
		}
	}

	if (meta->mines >= meta->rows * meta->cols) {
		fprintf(stderr, "%s: too many mines\n", argv[0]);
		exit(1);
	}
}

static void playing_click(SDL_Event *event, Ctx *ctx, bool *opening)
{
	const Sint32 x = event->button.x;
	const Sint32 y = event->button.y;
	size_t row, col;
	switch (event->button.button) {
		case SDL_BUTTON_LEFT: {
			coord_to_index(&ctx->game, x, y, &row, &col);
			click_tile(ctx, row, col, opening);
		} break;
		case SDL_BUTTON_RIGHT: {
			coord_to_index(&ctx->game, x, y, &row, &col);
			flag_tile(&ctx->game, row, col);
		} break;
	}
}

static void handle_click(SDL_Event *event, Ctx *ctx, bool *opening)
{
	switch (ctx->game.state) {
	case StatePlaying:
		playing_click(event, ctx, opening);
		break;
	case StateWin:
		break;
	case StateLose:
		break;
	}
}

/* This function is called every frame. */
static void frame_event(Ctx *ctx)
{
	switch (ctx->game.state) {
	case StatePlaying:
		break;
	case StateWin:
		break;
	case StateLose:
		break;
	}

	draw_grid(ctx);
}

int main(int argc, char **argv)
{
	GameMeta meta = {
		.rows = DEFAULT_ROWS,
		.cols = DEFAULT_COLS,
		.mines = DEFAULT_MINES,
	};

	if (argc > 1) parse_args(argc, argv, &meta);

	bool running = true;
	bool opening = true;

	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) printf("ERROR: initializing SDL: %s\n", SDL_GetError());
	if (TTF_Init() != 0) printf("ERROR: initializing SDL: %s\n", SDL_GetError());

	Ctx ctx = ctx_new(meta.rows, meta.cols, meta.mines);
	create_grid(&ctx.game, &opening);

	bool panned = false;
	bool hold_down = false;
	size_t delta_x, delta_y, start_x, start_y, mouse_x, mouse_y;

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
				case SDLK_r:
					create_grid(&ctx.game, &opening);
					break;
				}
				break;
			case SDL_QUIT:
				running = false;
				break;
			case SDL_MOUSEMOTION: {
				if (hold_down) {
					delta_x = start_x - event.motion.x;
					delta_y = start_y - event.motion.y;
					ctx.game.pan_x = delta_x;
					ctx.game.pan_y = delta_y;

					if (delta_x != 0 || delta_y != 0) {
						panned = true;
					}
				}
				mouse_x = event.motion.x;
				mouse_y = event.motion.y;
			} break;
			case SDL_MOUSEBUTTONDOWN: {
				start_x = event.motion.x + ctx.game.pan_x;
				start_y = event.motion.y + ctx.game.pan_y;
				hold_down = true;
			} break;
			case SDL_MOUSEBUTTONUP: {
				if (!panned) {
					handle_click(&event, &ctx, &opening);
				}
				hold_down = false;
				panned = false;
			} break;
			case SDL_MOUSEWHEEL: {
				if (event.wheel.y > 0) {
					ctx.game.scale += 0.1;
				} else {
					ctx.game.scale -= 0.1;
				}

#if 0				// TODO: Pan to mouse center
				size_t width_adj = WIDTH * ctx.game.scale;
				size_t height_adj = HEIGHT * ctx.game.scale;
				size_t x_adj = ctx.game.pan_x;
				size_t y_adj = ctx.game.pan_y;
				mouse_x = mouse_x * ctx.game.scale + ctx.game.pan_x;
				mouse_y = mouse_y * ctx.game.scale + ctx.game.pan_y;

				ctx.game.pan_x = mouse_x - width_adj / 2;
				ctx.game.pan_y = mouse_y - width_adj / 2;
#endif
			} break;
			}
		}

		frame_event(&ctx);

		SDL_RenderPresent(ctx.renderer);
		//SDL_Delay(1000 / FPS);
	}

	destroy_ctx(&ctx);
	SDL_Quit();
	TTF_Quit();
	return 0;
}
