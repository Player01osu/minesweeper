#include "colors.h"

Color u32_to_color(const Uint32 rgb)
{
	const Uint8 r = (rgb & 0xFF0000) >> 16;
	const Uint8 g = (rgb & 0x00FF00) >> 8;
	const Uint8 b = (rgb & 0x0000FF);

	const Color color = {
		r,
		g,
		b,
	};
	return color;
}

void u32_color(const Uint32 rgb, Uint8 *r, Uint8 *g, Uint8 *b)
{
	const Color color = u32_to_color(rgb);
	*r = color.r;
	*g = color.g;
	*b = color.b;
}

void set_render_color_u32(const Ctx *ctx, const Uint32 rgb, const Uint8 alpha)
{
	Uint8 r, g, b;
	u32_color(rgb, &r, &g, &b);
	SDL_SetRenderDrawColor(ctx->renderer, r, g, b, alpha);
}

