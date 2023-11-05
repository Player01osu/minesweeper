#include "mines.h"
#include <time.h>

bool is_valid_idx(const Game *game, size_t row, size_t col);

static size_t clamp(const size_t num, const size_t l, const size_t h)
{
	if (num < l)
		return l;
	if (num > h)
		return h;
	return num;
}

static Uint8 tile_is_mine(const Game *game, const Sint32 row, const Sint32 col)
{
	Tile **tiles = game->tiles;
	if (!is_valid_idx(game, row, col)) {
		return 0;
	}

	return tiles[row][col].mine;
}

static Uint8 tile_is_flagged(const Game *game, const Sint32 row, const Sint32 col)
{
	Tile **tiles = game->tiles;
	if (!is_valid_idx(game, row, col)) {
		return 0;
	}

	return tiles[row][col].flagged;
}

static Uint8 sum_surround_mines(Game *game, const Sint32 row, const Sint32 col)
{
	Tile **tiles = game->tiles;
	const size_t cols = game->cols;
	const size_t rows = game->rows;
	Uint8 sum = 0;

	/* Top row */
	sum += tile_is_mine(game, row - 1, (col - 1));
	sum += tile_is_mine(game, row - 1, (col - 0));
	sum += tile_is_mine(game, row - 1, (col + 1));

	/* Middle row */
	sum += tile_is_mine(game, row - 0, (col - 1));
	sum += tile_is_mine(game, row - 0, (col + 1));

	/* Bottom row */
	sum += tile_is_mine(game, row + 1, (col - 1));
	sum += tile_is_mine(game, row + 1, (col - 0));
	sum += tile_is_mine(game, row + 1, (col + 1));

	return sum;
}

// TODO: Generalize this (maybe function pointer)
Uint8 sum_surround_flagged(Game *game, const Sint32 row, const Sint32 col)
{
	Tile **tiles = game->tiles;
	const size_t cols = game->cols;
	const size_t rows = game->rows;
	Uint8 sum = 0;

	/* Top row */
	sum += tile_is_flagged(game, row - 1, (col - 1));
	sum += tile_is_flagged(game, row - 1, (col - 0));
	sum += tile_is_flagged(game, row - 1, (col + 1));

	/* Middle row */
	sum += tile_is_flagged(game, row - 0, (col - 1));
	sum += tile_is_flagged(game, row - 0, (col + 1));

	/* Bottom row */
	sum += tile_is_flagged(game, row + 1, (col - 1));
	sum += tile_is_flagged(game, row + 1, (col - 0));
	sum += tile_is_flagged(game, row + 1, (col + 1));

	return sum;
}

void calculate_surround(Game *game)
{
	Tile **tiles = game->tiles;
	const size_t cols = game->cols;
	const size_t rows = game->rows;

	for (size_t row = 0; row < rows; ++row) {
		for (size_t col = 0; col < cols; ++col) {
			if (tiles[row][col].mine)
				continue;
			tiles[row][col].surround_mines = sum_surround_mines(game, row, col);
		}
	}
}

void generate_mines(Game *game)
{
	Tile **tiles = game->tiles;
	const size_t cols = game->cols;
	const size_t rows = game->rows;

	//srand(time(0));
	for (size_t i = 0; i < game->mines; ++i) {
		size_t row = rand() % game->rows;
		size_t col = rand() % game->cols;

		while (tiles[row][col].mine) {
			col = (col + 1) % game->cols;
			if (col == 0) {
				row = (row + 1) % game->rows;
			}
		}
		tiles[row][col].mine = true;
	}

	calculate_surround(game);
}

void offset_mines(Game *game, const size_t row, const size_t col)
{
	Tile **tiles = game->tiles;
	const size_t cols = game->cols;
	const size_t rows = game->rows;

	Uint8 offset = 0;
	for (int i = 0; i < cols && tiles[row][(col + offset) % cols].mine; ++i) {
		++offset;
	}

	Tile tile_buf[cols];

	for (size_t x = 0; x < cols; ++x) {
		tile_buf[x].mine = tiles[row][(x + offset) % cols].mine;
	}

	for (size_t x = 0; x < cols; ++x) {
		tiles[row][x].mine = tile_buf[x].mine;
	}

	calculate_surround(game);
}
