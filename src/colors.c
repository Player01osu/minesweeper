#include "colors.h"
#include "state.h"

void set_render_color_u32(const Uint32 abgr)
{
	Uint8 r, g, b, a;
	SDL_Color color = *(SDL_Color *) &abgr;
	r = color.r;
	g = color.g;
	b = color.b;
	a = color.a;
	if (SDL_SetRenderDrawColor(renderer, r, g, b, a) < 0) {
		fprintf(stderr, "ERROR:Failed to set draw color:%s\n", SDL_GetError());
		exit(1);
	}
}

