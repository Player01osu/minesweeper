#ifndef CTX_H
#define CTX_H

#include "minesweeper.h"

void ctx_free(void);

void ctx_init(size_t rows, size_t cols, size_t mines);

#endif /* CTX_H */
