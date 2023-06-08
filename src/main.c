#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_pixels.h>

#define WIDTH 1280
#define HEIGHT 720

const Uint32 BACKGROUND_COLOR = 0x888888;
const Uint32 TILE_UNCLICKED_COLOR = 0x111111;
const Uint32 TILE_CLICKED_COLOR = 0xFFFFFF;

#define COLS 10
#define ROWS 10
const Uint32 PAD_INNER = 5;
const Uint32 PAD_OUTER = 30;

typedef struct {
	SDL_Rect rect;
	bool clicked;
} Tile;

Tile tiles[COLS][ROWS];

typedef struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
} Ctx;

typedef struct {
	Uint8 r;
	Uint8 g;
	Uint8 b;
} Color;

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
	Ctx ctx = {
		window,
		renderer,
	};
	return ctx;
}

Color u32_to_color(Uint32 rgb)
{
	Uint8 r = (rgb & 0xFF0000) >> 16;
	Uint8 g = (rgb & 0x00FF00) >> 8;
	Uint8 b = (rgb & 0x0000FF);

	Color color = {
		r,
		g,
		b,
	};
	return color;
}

void u32_color(Uint32 rgb, Uint8 *r, Uint8 *g, Uint8 *b)
{
	Color color = u32_to_color(rgb);
	*r = color.r;
	*g = color.g;
	*b = color.b;
}

void set_render_color_u32(Ctx ctx, Uint32 rgb, Uint8 alpha)
{
	Uint8 r, g, b;
	u32_color(rgb, &r, &g, &b);
	SDL_SetRenderDrawColor(ctx.renderer, r, g, b, alpha);
}

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
}

void draw_grid(Ctx ctx)
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			Tile tile = tiles[row][col];
			if (tile.clicked) {
				set_render_color_u32(ctx, TILE_CLICKED_COLOR, SDL_ALPHA_OPAQUE);
			} else {
				set_render_color_u32(ctx, TILE_UNCLICKED_COLOR, SDL_ALPHA_OPAQUE);
			}
			SDL_RenderFillRect(ctx.renderer, &tile.rect);
		}
	}
}

void toggle_tile(Ctx ctx, Uint32 x, Uint32 y)
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

void clear_background(Ctx ctx)
{
	set_render_color_u32(ctx, BACKGROUND_COLOR, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(ctx.renderer);
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
	/* control framerate */
	long then;
	float remainder;

	Ctx ctx = ctx_new();
	create_grid(ctx);

	bool running = true;
	while (running) {
		clear_background(ctx);
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
					toggle_tile(ctx, x, y);
				} break;
				}
			}
		}

		draw_grid(ctx);

		SDL_RenderPresent(ctx.renderer);
		capframerate(&then, &remainder);
	}

	destroy_ctx(ctx);
	SDL_Quit();
	return 0;
}
