#include "mines.h"
#include <time.h>

Uint8 tile_is_mine(const Sint32 row, const Sint32 col)
{
	Tile **tiles = game.tiles;
	if (!is_valid_idx(row, col)) return 0;

	return tiles[row][col].mine;
}

Uint8 tile_is_flagged(const Sint32 row, const Sint32 col)
{
	Tile **tiles = game.tiles;
	if (!is_valid_idx(row, col)) return 0;

	return tiles[row][col].state == TileStateFlagged;
}

Uint8 sum_surround(const Sint32 row, const Sint32 col,
		   Uint8 (*tile_count)(const Sint32, const Sint32))
{
	Uint8 sum = 0;

	/* Top row */
	sum += tile_count(row - 1, (col - 1));
	sum += tile_count(row - 1, (col - 0));
	sum += tile_count(row - 1, (col + 1));

	/* Middle row */
	sum += tile_count(row - 0, (col - 1));
	sum += tile_count(row - 0, (col + 1));

	/* Bottom row */
	sum += tile_count(row + 1, (col - 1));
	sum += tile_count(row + 1, (col - 0));
	sum += tile_count(row + 1, (col + 1));

	return sum;
}

void calculate_surround(void)
{
	Tile **tiles = game.tiles;
	const size_t cols = game.cols;
	const size_t rows = game.rows;

	for (size_t row = 0; row < rows; ++row) {
		for (size_t col = 0; col < cols; ++col) {
			if (tiles[row][col].mine) continue;
			tiles[row][col].surround_mines = sum_surround(row, col, tile_is_mine);
		}
	}
}

void generate_mines(void)
{
	Tile **tiles = game.tiles;

	//srand(time(0));
	for (size_t i = 0; i < game.mines; ++i) {
		size_t row = rand() % game.rows;
		size_t col = rand() % game.cols;

		while (tiles[row][col].mine) {
			col = (col + 1) % game.cols;
			if (col == 0) row = (row + 1) % game.rows;
		}
		tiles[row][col].mine = true;
	}

	calculate_surround();
}

void offset_mines(const size_t row, const size_t col)
{
	Tile **tiles = game.tiles;
	const size_t cols = game.cols;

	Uint8 offset = 0;
	for (size_t i = 0; i < cols && tiles[row][(col + offset) % cols].mine; ++i) {
		++offset;
	}

	Tile tile_buf[cols];

	for (size_t x = 0; x < cols; ++x) {
		tile_buf[x].mine = tiles[row][(x + offset) % cols].mine;
	}

	for (size_t x = 0; x < cols; ++x) {
		tiles[row][x].mine = tile_buf[x].mine;
	}

	calculate_surround();
}
