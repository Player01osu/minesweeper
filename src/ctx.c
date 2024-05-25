#include "ctx.h"

static void game_init(size_t rows, size_t cols, size_t mines)
{
	Tile **tiles = malloc(sizeof(*tiles) * (rows));
	for (size_t i = 0; i < rows; ++i) {
		tiles[i] = calloc(cols, sizeof(*tiles[i]));
	}

	game = (Game){
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
		.game_layout = {
			.w = WIDTH * 7 / 8,
			.h = HEIGHT * 7 / 8,
			.x = 0,
			.y = 0,
		}
	};
}

void ctx_free(void)
{
	Tile **tiles = game.tiles;
	for (size_t row = 0; row < game.rows; ++row) {
		free(tiles[row]);
	}
	free(tiles);

	for (size_t i = 0; i < 9; ++i) {
		SDL_DestroyTexture(text_ctx.num_texts[i]);
	}

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
}

void ctx_init(size_t rows, size_t cols, size_t mines)
{
	window = SDL_CreateWindow("TEST", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
			          WIDTH, HEIGHT, 0);
	if (!window) {
		fprintf(stderr, "Failed to init window\n");
		exit(1);
	}
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	if (!renderer) {
		fprintf(stderr, "ERROR:Failed to init renderer:%s\n", SDL_GetError());
		SDL_DestroyWindow(window);
		exit(1);
	}
	TTF_Font *font = TTF_OpenFont("/usr/share/fonts/TTF/FiraMono-Medium.ttf", 240);
	if (!font) {
		fprintf(stderr, "ERROR:Failed to init font:%s\n", SDL_GetError());
		SDL_DestroyWindow(window);
		SDL_DestroyRenderer(renderer);
		exit(1);
	}
	TTF_SetFontKerning(font, 1);
	text_ctx.font = font;

	SDL_Color white = { 255, 255, 255, 255 };

	char str[] = {'0', '\0'};
	for (size_t i = 0; i < 9; ++i) {
		*str = '0' + i;
		SDL_Surface *surface = TTF_RenderText_Solid(font, str, white);
		if (!surface) {
			fprintf(stderr, "ERROR:Failed to create surface:%s\n", SDL_GetError());
			exit(1);
		}
		SDL_Texture *num_text = SDL_CreateTextureFromSurface(renderer, surface);
		if (!num_text) {
			fprintf(stderr, "ERROR:Failed to create num texture:%s\n", SDL_GetError());
			exit(1);
		}
		text_ctx.num_texts[i] = num_text;
		SDL_FreeSurface(surface);
	}

	game_init(rows, cols, mines);
}
