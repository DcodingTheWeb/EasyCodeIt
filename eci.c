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

#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include "utils.h"
#include "parse.h"

int main(int argc, char *argv[]) {
	if (argc < 2) die("No arguments!");
	
	// Open the source file
	FILE *source_file = fopen(argv[1], "r");
	if (!source_file) die("Can't open source file! :(");
	
	// Read the source file
	char *code = readfile(source_file);
	if (!code) die("Failed to read from source file!");
	
	// Parse the code
	char *parse_error = parse(code);
	if (parse_error) {
		fputs("An error occured while parsing the code!\n", stderr);
		die(parse_error);
	}
	
	// Free the resources
	free(code);
	fclose(source_file);
	
	return EXIT_SUCCESS;
}
