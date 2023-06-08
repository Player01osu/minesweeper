#ifndef COLORS_H
#define COLORS_H

#include <SDL2/SDL.h>
#include "ctx.h"

typedef struct {
	Uint8 r;
	Uint8 g;
	Uint8 b;
} Color;

Color u32_to_color(Uint32 rgb);

void u32_color(Uint32 rgb, Uint8 *r, Uint8 *g, Uint8 *b);

void set_render_color_u32(Ctx *ctx, Uint32 rgb, Uint8 alpha);

#endif /* COLORS_H */
