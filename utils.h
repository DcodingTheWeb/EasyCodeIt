/* 
 * This file is part of EasyCodeIt.
 * 
 * Copyright (C) 2020 TheDcoder <TheDcoder@protonmail.com>
 * 
 * EasyCodeIt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdnoreturn.h>

#ifndef READ_FILE_BUFFER_SIZE
#define READ_FILE_BUFFER_SIZE 1024
#endif

struct ReadFileBufferNode {
	char buffer[READ_FILE_BUFFER_SIZE];
	size_t data_len;
	struct ReadFileBufferNode *next;
};

bool chrcmp(char chr, char *arr, size_t arr_len);
noreturn void die(char *msg);
char *readfile(FILE *file);

#endif
