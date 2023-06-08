#include "mines.h"

#define RAND_NUM 1
#define RAND_DENOM 6

size_t clamp(size_t num, size_t l, size_t h)
{
	if (num < l) return l;
	if (num > h) return h;
	return num;
}

Uint8 tile_is_mine(Tile tiles[ROWS][COLS], size_t row, size_t col)
{
	if (row < 0 || row >= ROWS || col < 0 || col >= COLS) return 0;

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
			if (tiles[row][col].mine) continue;
			tiles[row][col].surround_mines = sum_surround(tiles, row, col);
		}
	}
}

void generate_mines(Tile tiles[ROWS][COLS])
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			int mine = rand() % RAND_DENOM;

			if (mine < RAND_NUM) tiles[row][col].mine = true;
		}
	}

	calculate_surround(tiles);
}

void offset_mines(Tile tiles[ROWS][COLS], Uint32 row, Uint32 col)
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
