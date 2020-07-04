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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "utils.h"

bool chrcmp(char chr, char *arr, size_t arr_len) {
	bool present;
	for (size_t i = 0; i < arr_len; ++i) if (present = chr == arr[i]) break;
	return present;
}

noreturn void die(char *msg) {
	fputs(msg, stderr);
	if (*msg != '\0') fputs("\n", stderr);
	exit(EXIT_FAILURE);
}

char *readfile(FILE *file) {
	// Define the final buffer
	char *final_buffer = NULL;
	size_t final_size = 0;
	
	// Define and allocate the initial node
	struct ReadFileBufferNode *initial_node = malloc(sizeof(struct ReadFileBufferNode));
	if (!initial_node) return NULL;
	
	// Read the contents of file in chunks
	struct ReadFileBufferNode *curr_node = initial_node;
	struct ReadFileBufferNode *next_node;
	while (true) {
		// Copy the current chunk
		size_t bytes_read = fread(curr_node->buffer, 1, READ_FILE_BUFFER_SIZE, file);
		curr_node->data_len = bytes_read;
		final_size += bytes_read;
		if (bytes_read < READ_FILE_BUFFER_SIZE) {
			// Check if we have an error
			if (ferror(file)) goto cleanup;
			
			// Mark this node as final
			curr_node->next = NULL;
			
			// Break the loop
			break;
		}
		
		// Allocate the next buffer node
		next_node = malloc(sizeof(struct ReadFileBufferNode));
		if (!next_node) goto cleanup;
		curr_node->next = next_node;
		curr_node = next_node;
	}
	
	// Allocate the buffer
	final_buffer = malloc(final_size + 1);
	if (!final_buffer) goto cleanup;
	final_buffer[final_size] = '\0';
	
	// Copy data into the final buffer
	curr_node = initial_node;
	char *curr_chunk = final_buffer;
	do {
		memcpy(curr_chunk, curr_node->buffer, curr_node->data_len);
		curr_chunk += curr_node->data_len;
		curr_node = curr_node->next;
	} while (curr_node);
	
	// Free all nodes
	cleanup:
	curr_node = initial_node;
	do {
		next_node = curr_node->next;
		free(curr_node);
		curr_node = next_node;
	} while (curr_node);
	
	// Return the final buffer
	return final_buffer;
}
