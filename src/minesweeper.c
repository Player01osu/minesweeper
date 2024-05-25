#include <math.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_pixels.h>
#include <SDL2/SDL_ttf.h>

#include "state.h"
#include "minesweeper.h"
#include "colors.h"
#include "mines.h"

SDL_Window *window;
SDL_Renderer *renderer;
TextCtx text_ctx;
Game game;

char *program_name;
const char *help = "Usage: %s [OPTION]\n\n"
		   "--size [rows]x[cols]\n"
		   "--mines [number_of_mines]\n";

static void toggle_tile(const size_t row, const size_t col);

static void expand_cavern(const size_t row, const size_t col);

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

bool is_valid_idx(size_t row, size_t col)
{
	const size_t cols = game.cols;
	const size_t rows = game.rows;

	return col >= 0 && col < cols && row >= 0 && row < rows;
}

static GridCalc grid_calc(const SDL_Rect layout, const size_t rows, const size_t cols)
{
	const Uint32 min_canvas = MIN(layout.w, layout.h);

	const Uint32 width_adjusted = min_canvas - PAD_OUTER - PAD_INNER * cols;
	const Uint32 height_adjusted = min_canvas - PAD_OUTER - PAD_INNER * rows;

	const Uint32 rect_height = MIN(height_adjusted / rows, width_adjusted / cols);
	const Uint32 rect_width = MIN(height_adjusted / rows, width_adjusted / cols);

	const Uint32 board_width = cols * (rect_width + PAD_INNER);
	const Uint32 board_height = rows * (rect_height + PAD_INNER);

	const GridCalc calc = { rect_width, rect_height, board_width, board_height };
	return calc;
}

static SDL_Rect grid_tile(size_t row, size_t col)
{
	const GridCalc calc = grid_calc(game.game_layout, game.rows, game.cols);
	const Uint32 rect_width = calc.rect_width;
	const Uint32 rect_height = calc.rect_height;
	const Uint32 board_width = calc.board_width;
	const Uint32 board_height = calc.board_height;

	const Uint32 x = (WIDTH - board_width) / 2 + col * (rect_width + PAD_INNER);
	const Uint32 y = (HEIGHT - board_height) / 2 + row * (rect_height + PAD_INNER);

	return rect_new(x, y, rect_width, rect_height);
}

static void coord_to_index(const Sint32 x, const Sint32 y, size_t *row, size_t *col)
{
	const GridCalc calc = grid_calc(game.game_layout, game.rows, game.cols);
	const Uint32 rect_width = calc.rect_width;
	const Uint32 rect_height = calc.rect_height;
	const Uint32 board_width = calc.board_width;
	const Uint32 board_height = calc.board_height;

	*row = ((size_t)((y + game.pan_y) / game.scale) - ((HEIGHT - board_height) / 2)) / (rect_height + PAD_INNER);
	*col = ((size_t)((x + game.pan_x) / game.scale) - ((WIDTH - board_width) / 2)) / (rect_width + PAD_INNER);
}

static void lose_game(void)
{
	Tile *tile;
	game.state = StateLose;

	for_each_tile(tile) {
		if (tile->mine) tile->state = TileStateClicked;
	}
	printf("You Lost\n");
}

Tile *tile_at(const size_t row, const size_t col)
{
	Tile **tiles = game.tiles;
	if (!is_valid_idx(row, col)) return NULL;
	return &tiles[row][col];
}

static void click_tile(const size_t row, const size_t col)
{
	Tile *tile = tile_at(row, col);
	if (tile == NULL) return;

	switch (tile->state) {
	case TileStateUnclicked:
		if (opening) {
			offset_mines(row, col);
			opening = false;
		}

		toggle_tile(row, col);
		break;
	case TileStateClicked:
		if (sum_surround(row, col, tile_is_flagged) == tile->surround_mines) {
			expand_cavern(row, col);
		}
		break;
	case TileStateFlagged:
		tile->state = TileStateUnclicked;
		break;;
	}
}

static void toggle_tile(const size_t row, const size_t col)
{
	Tile *tile = tile_at(row, col);
	if (tile == NULL) return;

	switch (tile->state) {
	case TileStateClicked:
		break;
	case TileStateFlagged:
		break;
	case TileStateUnclicked:
		tile->state = TileStateClicked;

		if (tile->mine) {
			lose_game();
			return;
		}

		if (tile->surround_mines == 0) expand_cavern(row, col);

		++game.tiles_clicked;
		const size_t tiles_safe = game.rows * game.cols - game.mines;
		if (game.tiles_clicked == tiles_safe) {
			game.state = StateWin;
			printf("You Won\n");
			return;
		}
		break;
	}
}

static void flag_tile(size_t row, size_t col)
{
	if (!is_valid_idx(row, col)) return;

	Tile *tile = &game.tiles[row][col];

	switch (tile->state) {
	case TileStateClicked:
		break;
	case TileStateUnclicked:
		tile->state = TileStateFlagged;
		break;
	case TileStateFlagged:
		tile->state = TileStateUnclicked;
		break;
	}
}

static void expand_cavern(const size_t row, const size_t col)
{
	if (!is_valid_idx(row, col)) return;

	toggle_tile(row + 1, col + 1);
	toggle_tile(row + 1, col - 0);
	toggle_tile(row + 1, col - 1);

	toggle_tile(row + 0, col + 1);
	toggle_tile(row + 0, col - 1);

	toggle_tile(row - 1, col + 1);
	toggle_tile(row - 1, col - 0);
	toggle_tile(row - 1, col - 1);
}

static void create_grid(void)
{
	opening = false;
	game.state = StatePlaying;
	game.tiles_clicked = 0;
	size_t row, col;
	Tile *tile;
	for_each_enum_tile(row, col, tile) {
		*tile = (Tile){
			.rect = grid_tile(row, col),
			.state = TileStateUnclicked,
			.mine = false,
			.surround_mines = 0,
		};
	}
	generate_mines();
}
static void draw_grid(void)
{
	size_t row, col;
	Tile *tile;
	for_each_enum_tile(row, col, tile) {
		SDL_Rect rect = tile->rect;

		rect.w = ceil((float) tile->rect.w * game.scale);
		rect.h = ceil((float) tile->rect.h * game.scale);
		rect.x = ceil((float) tile->rect.x * game.scale - game.pan_x);
		rect.y = ceil((float) tile->rect.y * game.scale - game.pan_y);

		switch (tile->state) {
		case TileStateFlagged:
			if (game.mouse_row == row && game.mouse_col == col) {
				set_render_color_u32(TILE_FLAGGED_HIGHLIGHT_COLOR);
			} else {
				set_render_color_u32(TILE_FLAGGED_COLOR);
			}
			if (SDL_RenderFillRect(renderer, &rect) < 0) {
				fprintf(stderr, "ERROR: Failed to fill tile: %s", SDL_GetError());
				exit(1);
			}
			break;
		case TileStateClicked:
			if (tile->mine) {
				set_render_color_u32(TILE_CLICKED_MINE_COLOR);
				SDL_RenderFillRect(renderer, &rect);
				continue;
			}
			set_render_color_u32(TILE_CLICKED_COLOR);
			SDL_RenderFillRect(renderer, &rect);

			/* Don't draw number when tile isn't surrounded by mines */
			if (tile->surround_mines == 0) continue;

			SDL_RenderCopy(renderer, text_ctx.num_texts[tile->surround_mines], NULL, &rect);
			break;
		case TileStateUnclicked:
			if (game.mouse_row == row && game.mouse_col == col) {
				set_render_color_u32(TILE_UNCLICKED_HIGHLIGHT_COLOR);
			} else {
				set_render_color_u32(TILE_UNCLICKED_COLOR);
			}
			if (SDL_RenderFillRect(renderer, &rect) < 0) {
				fprintf(stderr, "ERROR: Failed to fill tile: %s", SDL_GetError());
				exit(1);
			}
			break;
		}
	}
}

static void clear_background(void)
{
	set_render_color_u32(BACKGROUND_COLOR);
	SDL_RenderClear(renderer);
}

static void parse_args_state(char *s, ParseState parse_state, GameMeta *meta)
{
	if (strlen(s) > 31) {
		fprintf(stderr, "%s: argument size too large '%s'\n", program_name, s);
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
			fprintf(stderr, "%s: Invalid size format '%s'\n", program_name, s);
			fprintf(stderr, help, program_name);
			exit(1);
		}
		++p;
		while (*p >= '0' && *p <= '9') {
			*ps_col = *p;
			++ps_col;
			++p;
		}
		if (*p != '\0') {
			fprintf(stderr, "%s: Invalid size format '%s'\n", program_name, s);
			fprintf(stderr, help, program_name);
			exit(1);
		}
		meta->rows = atoi(s_row);
		meta->cols = atoi(s_col);
		break;
	}
	case ParseStateMines: {
		char s_mines[32] = {0};
		char *ps_mines = s_mines;
		while (*p >= '0' && *p <= '9') {
			*ps_mines = *p;
			++ps_mines;
			++p;
		}
		if (*p != '\0') {
			fprintf(stderr, "%s: Invalid size format '%s'\n", program_name, s);
			fprintf(stderr, help, program_name);
			fprintf(stderr, "%c\n", *p);
			exit(1);
		}
		meta->mines = atoi(s_mines);
		break;
	}
	case ParseStateNone:
		fprintf(stderr, "%s: Ambiguous command '%s'\n", program_name, s);
		fprintf(stderr, help, program_name);
		exit(1);
		break;
	}
}

static void parse_args(int argc, char **argv, GameMeta *meta)
{
	ParseState parse_state = ParseStateNone;
	if (argc > 0) program_name = argv[0];

	for (size_t i = 1; i < (size_t)argc; ++i) {
		if (strncmp(argv[i], "--mines", 16) == 0) {
			parse_state = ParseStateMines;
		} else if (strncmp(argv[i], "--size", 16) == 0) {
			parse_state = ParseStateSize;
		} else if (argv[i][0] == '-') {
			fprintf(stderr, "%s: unrecognized option '%s'\n", program_name, argv[i]);
			fprintf(stderr, help, program_name);
			exit(1);
		} else {
			parse_args_state(argv[i], parse_state, meta);
		}
	}

	if (meta->mines >= meta->rows * meta->cols) {
		fprintf(stderr, "%s: too many mines\n", argv[0]);
		exit(1);
	}
}

static void playing_click(void)
{
	const Sint32 x = mouse_x();
	const Sint32 y = mouse_y();
	size_t row, col;
	if (is_leftup()) {
		coord_to_index(x, y, &row, &col);
		click_tile(row, col);
	} else {
		coord_to_index(x, y, &row, &col);
		flag_tile(row, col);
	}
}

static void handle_click(void)
{
	switch (game.state) {
	case StatePlaying:
		playing_click();
		break;
	case StateWin:
		break;
	case StateLose:
		break;
	}
}

/* This function is called every frame. */
static void frame_event(void)
{
	switch (game.state) {
	case StatePlaying:
		break;
	case StateWin:
		break;
	case StateLose:
		break;
	}

	SDL_RenderSetClipRect(renderer, NULL);
	draw_grid();
}

int main(int argc, char **argv)
{
	GameMeta meta = {
		.rows = DEFAULT_ROWS,
		.cols = DEFAULT_COLS,
		.mines = DEFAULT_MINES,
	};

	if (argc > 1) parse_args(argc, argv, &meta);

	if (SDL_Init(SDL_INIT_VIDEO) != 0) printf("ERROR: initializing SDL: %s\n", SDL_GetError());
	if (TTF_Init() != 0) printf("ERROR: initializing SDL: %s\n", SDL_GetError());

	ctx_init(meta.rows, meta.cols, meta.mines);
	create_grid();

	bool panned = false;
	bool hold = false;
	size_t delta_x, delta_y, start_x, start_y;
	running = true;
	opening = true;

	while (running) {
		clear_background();
		process_events();

		SDL_Scancode scancode;
		while (key_iter(&scancode)) {
			switch (scancode) {
			case SDLK_q:
				fprintf(stderr, "Attempting to quit...\n");
				running = false;
				break;
			case SDLK_r:
				create_grid();
				break;
			default: ;
			}
		}

		if (mouse_moved()) {
			int x = mouse_x();
			int y = mouse_y();
			coord_to_index(x, y, &game.mouse_row, &game.mouse_col);
			if (hold) {
				delta_x = start_x - x;
				delta_y = start_y - y;
				game.pan_x = delta_x;
				game.pan_y = delta_y;

				panned = delta_x != 0 || delta_y != 0;
			}
		}

		if (is_leftdown() && !hold) {
			start_x = mouse_x() + game.pan_x;
			start_y = mouse_y() + game.pan_y;
			hold = true;
		}

		if (is_leftup() || is_rightup()) {
			if (!panned) handle_click();
			panned = false;
			hold = false;
		}

		game.scale += wheel_y() * 0.1;

#if 0				// TODO: Pan to mouse center
			size_t width_adj = WIDTH * game.scale;
			size_t height_adj = HEIGHT * game.scale;
			size_t x_adj = game.pan_x;
			size_t y_adj = game.pan_y;
			mouse_x = mouse_x * game.scale + game.pan_x;
			mouse_y = mouse_y * game.scale + game.pan_y;

			game.pan_x = mouse_x - width_adj / 2;
			game.pan_y = mouse_y - width_adj / 2;
#endif

		frame_event();

		SDL_RenderPresent(renderer);
		//SDL_Delay(1000 / FPS);
	}

	ctx_free();
	SDL_Quit();
	TTF_Quit();
	return 0;
}
