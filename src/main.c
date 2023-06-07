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

const Uint32 SQUARE_COLOR = 0x111111;
const Uint32 BACKGROUND_COLOR = 0x888888;

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

const Uint32 COLS = 10;
const Uint32 ROWS = 10;
const Uint32 PAD_INNER = 5;
const Uint32 PAD_OUTER = 30;

void create_grid(Ctx ctx)
{
	Uint8 r, g, b;
	u32_color(SQUARE_COLOR, &r, &g, &b);

	SDL_SetRenderDrawColor(ctx.renderer, r, g, b, SDL_ALPHA_OPAQUE);

	Uint32 width = WIDTH;
	Uint32 height = HEIGHT;

	Uint32 min_canvas = width > height ? height : width;

	Uint32 pad_outer = PAD_OUTER;
	Uint32 pad_inner = PAD_INNER;

	Uint32 rows = ROWS;
	Uint32 cols = COLS;

	Uint32 width_adjusted = min_canvas - pad_outer - pad_inner * cols;
	Uint32 height_adjusted = min_canvas - pad_outer - pad_inner * rows;

	Uint32 rect_width = width_adjusted / cols;
	Uint32 rect_height = height_adjusted / rows;

	for (size_t row = 0; row < rows; ++row) {
		for (size_t col = 0; col < cols; ++col) {
			SDL_Rect rect = rect_new(
				(width / 2 - pad_outer) / 2 + col * (rect_width + pad_inner),
				(pad_outer) / 2 + row * (rect_height + pad_inner),
				rect_width,
				rect_height
			);
			SDL_RenderFillRect(ctx.renderer, &rect);
		}
	}

}

void clear_background(Ctx ctx)
{
	Uint8 r, g, b;
	u32_color(BACKGROUND_COLOR, &r, &g, &b);

	SDL_SetRenderDrawColor(ctx.renderer, r, g, b, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(ctx.renderer);
}

int main(int argc, char **argv)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		printf("error initializing SDL: %s\n", SDL_GetError());
	}
	Ctx ctx = ctx_new();

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
			}
		}

		create_grid(ctx);

		SDL_RenderPresent(ctx.renderer);
	}

	destroy_ctx(ctx);
	SDL_Quit();
	return 0;
}
