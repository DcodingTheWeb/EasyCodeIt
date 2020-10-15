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

#ifndef PARSE_H
#define PARSE_H

#include <stdbool.h>
#include <stddef.h>

enum TokenType {
	TOK_UNKNOWN,
	TOK_WHITESPACE,
	TOK_COMMENT,
	TOK_DIRECTIVE,
	TOK_NUMBER,
	TOK_STRING,
	TOK_WORD,
	TOK_MACRO,
	TOK_VARIABLE,
	TOK_OPERATOR,
	TOK_BRACKET,
	TOK_DOT,
	TOK_COMMA,
};

enum Keyword {
	KWD_DIM,
	KWD_LOCAL,
	KWD_GLOBAL,
	KWD_ENUM,
	KWD_CONST,
	KWD_STATIC,
	KWD_CONT_CASE,
	KWD_CONT_LOOP,
	KWD_DEFAULT,
	KWD_NULL,
	KWD_DO,
	KWD_UNTIL,
	KWD_WHILE,
	KWD_END_WHILE,
	KWD_FOR,
	KWD_IN,
	KWD_TO,
	KWD_STEP,
	KWD_NEXT,
	KWD_EXIT,
	KWD_EXITLOOP,
	KWD_FUNC,
	KWD_RETURN,
	KWD_END_FUNC,
	KWD_IF,
	KWD_ELSE,
	KWD_ELSE_IF,
	KWD_END_IF,
	KWD_REDIM,
	KWD_SELECT,
	KWD_SWITCH,
	KWD_CASE,
	KWD_END_SELECT,
	KWD_END_SWITCH,
};

struct Token {
	enum TokenType type;
	char *data;
	size_t data_len;
	void *info;
};

struct TokenList {
	size_t length;
	struct TokenListNode *head;
	struct TokenListNode *tail;
	bool dirty;
};

struct TokenListNode {
	struct Token *token;
	struct TokenListNode *prev;
	struct TokenListNode *next;
};

void parse(char *code);
struct Token token_get(char *code, char **next);
struct TokenList token_get_list(char *code);
size_t scan_string(char *str, bool (cmpfunc)(char));

bool char_is_whitespace(char chr);
bool char_is_alpha(char chr);
bool char_is_num(char chr);
bool char_is_alphanum(char chr);
bool char_is_opsym(char chr);
bool char_is_bracket(char chr);
bool char_is_not_eol(char chr);

#endif
