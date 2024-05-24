#ifndef STATE_H
#define STATE_H

#include <inttypes.h>
#include <stdbool.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

extern bool running;
extern bool opening;

void process_events(void);

int wheel_y(void);
int mouse_x(void);
int mouse_y(void);
bool mouse_moved(void);
bool is_leftdown(void);
bool is_leftup(void);
bool is_rightdown(void);
bool is_rightup(void);
bool is_keydown(SDL_Scancode key);
bool key_iter(SDL_Scancode *scancode);

#endif
