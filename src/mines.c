#include "mines.h"
#include <time.h>

bool is_valid_idx(size_t row, size_t col);

size_t clamp(size_t num, size_t l, size_t h)
{
	if (num < l)
		return l;
	if (num > h)
		return h;
	return num;
}

Uint8 tile_is_mine(Tile tiles[ROWS][COLS], size_t row, size_t col)
{
	if (!is_valid_idx(row, col))
		return 0;

	return tiles[row][col].mine;
}

/* + + + + +
 * + + + + +
 * + + + + +
 * + + + + +
 */
Uint8 sum_surround(Tile tiles[ROWS][COLS], size_t row, size_t col)
{
	Uint8 sum = 0;

	/* Top row */
	sum += tile_is_mine(tiles, row - 1, (col - 1));
	sum += tile_is_mine(tiles, row - 1, (col - 0));
	sum += tile_is_mine(tiles, row - 1, (col + 1));

	/* Middle row */
	sum += tile_is_mine(tiles, row - 0, (col - 1));
	sum += tile_is_mine(tiles, row - 0, (col + 1));

	/* Bottom row */
	sum += tile_is_mine(tiles, row + 1, (col - 1));
	sum += tile_is_mine(tiles, row + 1, (col - 0));
	sum += tile_is_mine(tiles, row + 1, (col + 1));

	return sum;
}

void calculate_surround(Tile tiles[ROWS][COLS])
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			if (tiles[row][col].mine)
				continue;
			tiles[row][col].surround_mines = sum_surround(tiles, row, col);
		}
	}
}

void generate_mines(const Game *game, Tile tiles[ROWS][COLS])
{
	//srand(time(0));
	for (size_t i = 0; i < game->mines; ++i) {
		size_t row = rand() % game->rows;
		size_t col = rand() % game->cols;

		// FIXME: Could hang if whole row is filled with mines.
		while (tiles[row][col].mine) {
			col = (col + 1) % game->cols;
		}
		tiles[row][col].mine = true;
	}

	calculate_surround(tiles);
}

void offset_mines(Tile tiles[ROWS][COLS], size_t row, size_t col)
{
	Uint8 offset = 0;
	while (tiles[row][(col + offset) % COLS].mine) {
		++offset;
	}

	Tile tile_buf[COLS];

	for (size_t x = 0; x < COLS; ++x) {
		tile_buf[x].mine = tiles[row][(x + offset) % COLS].mine;
	}

	for (size_t x = 0; x < COLS; ++x) {
		tiles[row][x].mine = tile_buf[x].mine;
	}

	calculate_surround(tiles);
}
