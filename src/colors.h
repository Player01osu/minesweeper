#ifndef COLORS_H
#define COLORS_H

#include <SDL2/SDL.h>
#include "ctx.h"

// TODO Use SDL builtin color struct.
typedef struct {
	const Uint8 r;
	const Uint8 g;
	const Uint8 b;
} Color;

Color u32_to_color(const Uint32 rgb);

void u32_color(const Uint32 rgb, Uint8 *r, Uint8 *g, Uint8 *b);

void set_render_color_u32(const Ctx *ctx, const Uint32 rgb, const Uint8 alpha);

#endif /* COLORS_H */
