#ifndef COLORS_H
#define COLORS_H

#include <SDL2/SDL.h>
#include "ctx.h"

#define u32_to_color(rgba) (*(SDL_Color *) (rgba))

void set_render_color_u32(const Uint32 abgr);

#endif /* COLORS_H */
