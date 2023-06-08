#include "colors.h"

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

void set_render_color_u32(Ctx *ctx, Uint32 rgb, Uint8 alpha)
{
	Uint8 r, g, b;
	u32_color(rgb, &r, &g, &b);
	SDL_SetRenderDrawColor(ctx->renderer, r, g, b, alpha);
}

