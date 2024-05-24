#include "state.h"

bool running;
bool opening;

static int key_iter_idx;
static SDL_Scancode key_iter_map[SDL_NUM_SCANCODES];
static bool _mouse_moved;
static bool leftdown;
static bool leftup;
static bool rightdown;
static bool rightup;
static int _mouse_x = -1;
static int _mouse_y = -1;
static int _wheel_y;

int wheel_y(void)
{
	return _wheel_y;
}

int mouse_x(void)
{
	return _mouse_x;
}

int mouse_y(void)
{
	return _mouse_y;
}

bool mouse_moved(void)
{
	return _mouse_moved;
}

bool is_leftdown(void)
{
	return leftdown;
}

bool is_leftup(void)
{
	return leftup;
}

bool is_rightdown(void)
{
	return rightdown;
}

bool is_rightup(void)
{
	return rightup;
}

bool key_iter(SDL_Scancode *scancode)
{
	static int i = 0;
	if (i < key_iter_idx) {
		*scancode = key_iter_map[i];
		++i;
		return true;
	} else {
		i = 0;
		return false;
	}
}

void process_events(void)
{
	_mouse_moved = false;
	leftdown = false;
	leftup = false;
	rightdown = false;
	rightup = false;
	key_iter_idx = 0;
	_wheel_y = 0;

	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		switch (event.type) {
		case SDL_KEYDOWN:
			key_iter_map[key_iter_idx++] = event.key.keysym.sym;
			break;
		case SDL_QUIT:
			running = false;
			break;
		case SDL_MOUSEMOTION:
			_mouse_moved = true;
			_mouse_x = event.motion.x;
			_mouse_y = event.motion.y;
			break;
		case SDL_MOUSEBUTTONDOWN:
			if (event.button.button == 1) {
				leftdown = true;
			} else if (event.button.button == 3) {
				rightdown = true;
			}
			break;
		case SDL_MOUSEBUTTONUP:
			if (event.button.button == 1) {
				leftup = true;
			} else if (event.button.button == 3) {
				rightup = true;
			}
			break;
		case SDL_MOUSEWHEEL:
			_wheel_y = event.wheel.y;
			break;
		}
	}
}
