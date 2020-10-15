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

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parse.h"
#include "utils.h"

const char CHR_COMMENT = ';';
const char CHR_DIRECTIVE = '#';
const char CHR_MACRO = '@';
const char CHR_VARIABLE = '$';
const char CHR_DOT = '.';
const char CHR_COMMA = ',';

char CHRSET_WHITESPACE[] = {' ', '\t', '\r', '\n'};
char CHRSET_QUOTE[] = {'\'', '"'};
char CHRSET_OPERATOR[] = {
	'+', '-', '*', '/', '^',
	'&',
	'=', '<', '>',
	'?', ':',
};
char CHRSET_OPERATOR_EQUABLE[] = {'+', '-', '*', '/', '^', '&', '='};
char CHRSET_BRACKET[] = {'[', ']', '(', ')'};

char STRING_CS[] = "cs";
char STRING_CE[] = "ce";
char STRING_COMMENT_START[] = "comments-start";
char STRING_COMMENT_END[] = "comments-end";

struct KeywordMap {
	char *string;
	enum Keyword symbol;
};

struct KeywordMap KEYWORD_MAP[] = {
	{"Dim", KWD_DIM},
	{"Local", KWD_LOCAL},
	{"Global", KWD_GLOBAL},
	{"Enum", KWD_ENUM},
	{"Const", KWD_CONST},
	{"Static", KWD_STATIC},
	{"ContinueCase", KWD_CONT_CASE},
	{"ContinueLoop", KWD_CONT_LOOP},
	{"Default", KWD_DEFAULT},
	{"Null", KWD_NULL},
	{"Do", KWD_DO},
	{"Until", KWD_UNTIL},
	{"While", KWD_WHILE},
	{"WEnd", KWD_END_WHILE},
	{"For", KWD_FOR},
	{"In", KWD_IN},
	{"To", KWD_TO},
	{"Step", KWD_STEP},
	{"Next", KWD_NEXT},
	{"Exit", KWD_EXIT},
	{"ExitLoop", KWD_EXITLOOP},
	{"Func", KWD_FUNC},
	{"Return", KWD_RETURN},
	{"EndFunc", KWD_END_FUNC},
	{"If", KWD_IF},
	{"Else", KWD_ELSE},
	{"ElseIf", KWD_ELSE_IF},
	{"EndIf", KWD_END_IF},
	{"ReDim", KWD_REDIM},
	{"Select", KWD_SELECT},
	{"Switch", KWD_SWITCH},
	{"Case", KWD_CASE},
	{"EndSelect", KWD_END_SELECT},
	{"EndSwitch", KWD_END_SWITCH},
};

static void print_token(struct Token *token) {
	puts("---### TOKEN ###---");
	char *token_type;
	switch (token->type) {
		case TOK_UNKNOWN:
			token_type = "Unknown";
			break;
		case TOK_WHITESPACE:
			token_type = "Whitespace";
			break;
		case TOK_COMMENT:
			token_type = "Comment";
			break;
		case TOK_DIRECTIVE:
			token_type = "Directive";
			break;
		case TOK_NUMBER:
			token_type = "Number";
			break;
		case TOK_STRING:
			token_type = "String";
			break;
		case TOK_WORD:
			token_type = "Word";
			break;
		case TOK_MACRO:
			token_type = "Macro";
			break;
		case TOK_VARIABLE:
			token_type = "Variable";
			break;
		case TOK_OPERATOR:
			token_type = "Operator";
			break;
		case TOK_BRACKET:
			token_type = "Bracket";
			break;
		case TOK_DOT:
			token_type = "Dot";
			break;
		case TOK_COMMA:
			token_type = "Comma";
			break;
		default:
			token_type = "Unnamed";
			break;
	}
	fputs("Type: ", stdout);
	puts(token_type);
	fputs("Data: ", stdout);
	size_t i = 0;
	switch (token->type) {
		case TOK_WORD:
			if (!token->info) goto print_raw_data;
			fputs("<Keyword>", stdout);
			while (KEYWORD_MAP[i].symbol != *(enum Keyword *)token->info) ++i;
			fputs(KEYWORD_MAP[i].string, stdout);
			break;
		default:
			print_raw_data:
			for (size_t c = 0; c < token->data_len; c++) putchar(token->data[c]);
	}
	putchar('\n');
}

void parse(char *code) {
	struct TokenList token_list = token_get_list(code);
	if (!token_list.length) return;
	struct TokenListNode *token_list_node = token_list.head;
	if (token_list.dirty) fputs("!!! WARNING: Unknown token(s) encountered !!!\n", stderr);
	do {
		struct Token *token = token_list_node->token;
		if (token->type != TOK_WHITESPACE) print_token(token);
		token_list_node = token_list_node->next;
	} while (token_list_node);
}

struct Token token_get(char *code, char **next) {
	struct Token token = {
		.type = TOK_UNKNOWN,
		.data = NULL,
		.data_len = 0,
		.info = NULL,
	};
	size_t length;
	char *next_code = NULL;
	
	// Identify the token
	if (length = scan_string(code, char_is_whitespace)) {
		// Whitespace
		token.type = TOK_WHITESPACE;
		token.data = code;
		token.data_len = length;
	} else if (*code == CHR_COMMENT || *code == CHR_DIRECTIVE) {
		// Comment or Directive
		token.type = *code == CHR_COMMENT ? TOK_COMMENT : TOK_DIRECTIVE;
		token.data = ++code;
		token.data_len = scan_string(code, char_is_not_eol);
		
		// Check if this is a multi-line comment
		bool multiline_comment = false;
		if (token.type == TOK_DIRECTIVE) {
			bool match_short, match_long = false;
			match_short = strncasecmp(STRING_CS, code, (sizeof STRING_CS) - 1) == 0;
			if (!match_short) match_long = strncasecmp(STRING_COMMENT_START, code, (sizeof STRING_COMMENT_START) - 1) == 0;
			// Make sure we have a whitespace after the directive
			char *comment_start;
			if (match_long || match_short) {
				comment_start = code + (match_long ? sizeof STRING_COMMENT_START : sizeof STRING_CS);
				multiline_comment = char_is_whitespace(comment_start[-1]);
			}
			if (multiline_comment) {
				token.type = TOK_COMMENT;
				token.data = code = comment_start;
			}
		}
		
		if (multiline_comment) {
			// Scan for the ending directive token
			char *comment_end;
			while (true) {
				while (*++code != '\0') if (*code == CHR_DIRECTIVE) break;
				if (*code == '\0') break;
				
				bool match_short, match_long = false, match = false;
				++code;
				match_short = strncasecmp(STRING_CE, code, (sizeof STRING_CE) - 1) == 0;
				if (!match_short) match_long = strncasecmp(STRING_COMMENT_END, code, (sizeof STRING_COMMENT_END) - 1) == 0;
				// Make sure we have a whitespace after the directive
				if (match_long || match_short) {
					comment_end = code + ((match_long ? sizeof STRING_COMMENT_END : sizeof STRING_CE) - 1);
					match = char_is_whitespace(*comment_end);
				}
				if (match) break;
			}
			token.data_len = (code - token.data) - 1;
			next_code = comment_end;
		} else {
			token.data_len = scan_string(code, char_is_not_eol);
		}
	} else if (length = scan_string(code, char_is_num)){
		// Number
		token.type = TOK_NUMBER;
		token.data = code;
		token.data_len = length;
	} else if (chrcmp(*code, CHRSET_QUOTE, sizeof CHRSET_QUOTE)) {
		// String
		token.type = TOK_STRING;
		const char quote = *code;
		token.data = code + 1;
		for (token.data_len = 0; token.data[token.data_len] != quote; ++token.data_len);
		next_code = token.data + token.data_len + 1;
	} else if (length = scan_string(code, char_is_alphanum)){
		// Word
		token.type = TOK_WORD;
		token.data = code;
		token.data_len = length;
		
		// Identify keywords
		for (size_t i = 0; i < sizeof KEYWORD_MAP / sizeof(struct KeywordMap); ++i) if (strncmp(KEYWORD_MAP[i].string, code, length) == 0) {
			token.info = &(KEYWORD_MAP[i].symbol);
			break;
		}
	} else if (*code == CHR_MACRO || *code == CHR_VARIABLE){
		// Macro or Variable
		token.type = *code == CHR_MACRO ? TOK_MACRO : TOK_VARIABLE;
		token.data = ++code;
		token.data_len = scan_string(code, char_is_alphanum);
	} else if (char_is_opsym(*code)) {
		// Operator
		token.type = TOK_OPERATOR;
		token.data = code;
		
		// Include the trailing `=` if possible
		token.data_len = code[1] == '=' && chrcmp(*code, CHRSET_OPERATOR_EQUABLE, sizeof CHRSET_OPERATOR_EQUABLE) ? 2 : 1;
	} else if (char_is_bracket(*code)) {
		// Bracket (Parenthesis)
		token.type = TOK_BRACKET;
		token.data = code;
		token.data_len = 1;
	} else if (*code == CHR_DOT) {
		// Dot (Full Stop)
		token.type = TOK_DOT;
		token.data = code;
		token.data_len = 1;
	} else if (*code == CHR_COMMA) {
		// Comma
		token.type = TOK_COMMA;
		token.data = code;
		token.data_len = 1;
	} else {
		// Unknown
		token.data = code;
		token.data_len = 1;
	}
	
	// Set the next code
	if (next_code) {
		*next = *next_code == '\0' ? NULL : next_code;
	} else {
		code += token.data_len;
		*next = *code == '\0' ? NULL : code;
	}
	
	// Return the token
	return token;
}

struct TokenList token_get_list(char *code) {
	struct TokenList list = {.length = 0, .dirty = false};
	
	while (code) {
		struct Token *token = malloc(sizeof(struct Token));
		if (!token) goto end;
		
		*token = token_get(code, &code);
		if (token->type == TOK_UNKNOWN) list.dirty = true;
		
		struct TokenListNode *new_node = malloc(sizeof(struct TokenListNode));
		if (!new_node) goto end;
		if (list.tail) {
			list.tail->next = new_node;
			new_node->prev = list.tail;
			list.tail = new_node;
		} else {
			list.head = new_node;
			list.tail = new_node;
			new_node->prev = NULL;
		}
		new_node->token = token;
		new_node->next = NULL;
		list.length += 1;
	}
	
	end: return list;
};

size_t scan_string(char *str, bool (cmpfunc)(char)) {
	size_t len = 0;
	while (true) {
		if (!cmpfunc(*str)) break;
		++len; ++str;
	}
	return len;
}

bool char_is_whitespace(char chr) {
	return chrcmp(chr, CHRSET_WHITESPACE, sizeof CHRSET_WHITESPACE);
}

bool char_is_alpha(char chr) {
	return isalpha(chr);
}

bool char_is_num(char chr) {
	return isdigit(chr);
}

bool char_is_alphanum(char chr) {
	return char_is_alpha(chr) || char_is_num(chr) || chr == '_';
}

bool char_is_opsym(char chr) {
	return chrcmp(chr, CHRSET_OPERATOR, sizeof CHRSET_OPERATOR);
}

bool char_is_bracket(char chr) {
	return chrcmp(chr, CHRSET_BRACKET, sizeof CHRSET_BRACKET);
}

bool char_is_not_eol(char chr) {
	return chr != '\n' && chr != '\0';
}
