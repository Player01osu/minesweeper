#include "mines.h"

#define RAND_NUM 1
#define RAND_DENOM 2

size_t clamp(size_t num, size_t l, size_t h)
{
	if (num < l) return l;
	if (num > h) return h;
	return num;
}

void generate_mines(Tile tile[ROWS][COLS])
{
	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			int mine = rand() % RAND_DENOM;
			tile[row][col].mine = mine;
		}
	}

	for (size_t row = 0; row < ROWS; ++row) {
		for (size_t col = 0; col < COLS; ++col) {
			Uint8 sum = 0;

			if (tile[row][col].mine) continue;

			/* Top row */
			sum += tile[clamp(row - 1, 0, ROWS)][clamp(col - 1, 0, COLS)].mine;
			sum += tile[clamp(row - 1, 0, ROWS)][clamp(col - 0, 0, COLS)].mine;
			sum += tile[clamp(row - 1, 0, ROWS)][clamp(col + 1, 0, COLS)].mine;

			/* Middle row */
			sum += tile[clamp(row - 0, 0, ROWS)][clamp(col - 1, 0, COLS)].mine;
			sum += tile[clamp(row - 0, 0, ROWS)][clamp(col + 1, 0, COLS)].mine;

			/* Bottom row */
			sum += tile[clamp(row + 1, 0, ROWS)][clamp(col - 1, 0, COLS)].mine;
			sum += tile[clamp(row + 1, 0, ROWS)][clamp(col - 0, 0, COLS)].mine;
			sum += tile[clamp(row + 1, 0, ROWS)][clamp(col + 1, 0, COLS)].mine;

			tile[row][col].surround_mines = sum;
		}
	}
	// Mark mines
	// Each non-mine, check surrounding summing up all mines
}

void offset_mines(void);
