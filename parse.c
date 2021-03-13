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

#define _GNU_SOURCE /* Required to enable (v)asprintf */

#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
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

enum Keyword KWDSET_DECLARATOR[] = {
	KWD_FUNC,
	KWD_GLOBAL,
	KWD_LOCAL,
	KWD_STATIC,
	KWD_CONST,
};

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
	{"And", KWD_AND},
	{"Or", KWD_OR},
	{"Not", KWD_NOT},
};

struct {
	jmp_buf jump;
	char *msg;
	bool free_msg;
} parse_error = {.free_msg = false};

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
			if (token->keyword == KWD_NONE) goto print_raw_data;
			fputs("<Keyword>", stdout);
			while (KEYWORD_MAP[i].symbol != token->keyword) ++i;
			fputs(KEYWORD_MAP[i].string, stdout);
			break;
		default:
			print_raw_data:
			for (size_t c = 0; c < token->data_len; c++) putchar(token->data[c]);
	}
	putchar('\n');
}

bool parse(char *code) {
	if (setjmp(parse_error.jump)) return false;
	
	struct TokenList token_list = token_get_list(code);
	if (!token_list.length) raise_mem("generating token list");
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
		
		// Include the fractional part if present
		if (code[length] == '.') length += scan_string(code + length + 1, char_is_num) + 1;
		
		token.data_len = length;
		
		// Parse the number
		char boundary_char = code[length];
		code[length] = '\0';
		token.number = strtod(code, NULL);
		code[length] = boundary_char;
	} else if (chrcmp(*code, CHRSET_QUOTE, sizeof CHRSET_QUOTE)) {
		// String
		token.type = TOK_STRING;
		token.quote = *code;
		token.data = code + 1;
		for (token.data_len = 0; token.data[token.data_len] != '\0' && (token.data[token.data_len] != token.quote || (token.data[token.data_len + 1] == token.quote ? ++token.data_len : false)); ++token.data_len);
		next_code = token.data + token.data_len + 1;
	} else if (length = scan_string(code, char_is_alphanum)){
		// Word
		token.type = TOK_WORD;
		token.data = code;
		token.data_len = length;
		token.keyword = KWD_NONE;
		
		// Identify keywords
		for (size_t i = 0; i < sizeof KEYWORD_MAP / sizeof(struct KeywordMap); ++i) if (strncmp(KEYWORD_MAP[i].string, code, length) == 0) {
			token.info = &(KEYWORD_MAP[i].symbol);
			token.keyword = KEYWORD_MAP[i].symbol;
			break;
		}
		
		// Special: Convert "And" and "Or" to operators
		if (token.keyword == KWD_AND) {
			token.type = TOK_OPERATOR;
			token.op_info.sym = OPR_AND;
			token.op_info.op = OP_AND;
		} else if (token.keyword == KWD_OR) {
			token.type = TOK_OPERATOR;
			token.op_info.sym = OPR_OR;
			token.op_info.op = OP_OR;
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
		token.op_info.sym = opsym_to_opr(*code);
		
		// Include the trailing `=` if possible
		bool equable = code[1] == '=' && chrcmp(*code, CHRSET_OPERATOR_EQUABLE, sizeof CHRSET_OPERATOR_EQUABLE);
		if (equable) {
			token.data_len = 2;
			//token.equal_op = token.op;
			token.op_info.sym = OPR_EQU;
			token.op_info.op = OP_EQU;
		} else {
			token.data_len = 1;
		}
		
		// Assign the operation
		if (equable) {
			enum Operator equal_opr = opsym_to_opr(code[1]);
			token.op_info.equal_op = opr_to_op(equal_opr);
		} else {
			token.op_info.op = opr_to_op(token.op_info.sym);
		}
		
		// Assign the precedence
		token.op_info.precedence = op_to_precedence(token.op_info.op);
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

struct Token *token_list_to_array(struct TokenList *list, bool pad) {
	struct Token *tokens = malloc(sizeof(struct Token) * (list->length + (pad ? 2 : 0)));
	if (!tokens) return NULL;
	if (pad) /* Reserve first element for padding */ ++tokens;
	
	struct TokenListNode *node = list->head;
	for (size_t i = 0; i < list->length; ++i) {
		tokens[i] = *node->token;
		node = node->next;
	}
	
	if (pad) {
		// Apply padding
		//struct Token padding = {.type = TOK_EOF};
		tokens[list->length] = (struct Token){
			.type = TOK_EOF,
			.data = list->tail->token->data + list->tail->token->data_len,
			.data_len = 0,
		};
		*--tokens = (struct Token){
			.type = TOK_EOF,
			.data = list->head->token->data,
			.data_len = 0,
		};
	}
	
	return tokens;
}

enum Operator opsym_to_opr(char sym) {
	enum Operator opr = OPR_ERR;
	switch (sym) {
		case '+':
			opr = OPR_ADD;
			break;
		case '-':
			opr = OPR_SUB;
			break;
		case '*':
			opr = OPR_MUL;
			break;
		case '/':
			opr = OPR_DIV;
			break;
		case '^':
			opr = OPR_EXP;
			break;
		case '&':
			opr = OPR_CAT;
			break;
		case '=':
			opr = OPR_EQU;
			break;
		case '<':
			opr = OPR_LES;
			break;
		case '>':
			opr = OPR_GRT;
			break;
		case '?':
			opr = OPR_CON_MRK;
			break;
		case ':':
			opr = OPR_CON_SEP;
			break;
	}
	return opr;
}

enum Operation opr_to_op(enum Operator opr) {
	enum Operation op = OP_ERR;
	switch (opr) {
		case OPR_ADD:
			op = OP_ADD;
			break;
		case OPR_SUB:
			op = OP_SUB;
			break;
		case OPR_MUL:
			op = OP_MUL;
			break;
		case OPR_DIV:
			op = OP_DIV;
			break;
		case OPR_EXP:
			op = OP_EXP;
			break;
		case OPR_CAT:
			op = OP_CAT;
			break;
		case OPR_EQU:
			op = OP_EQU;
			break;
		case OPR_LES:
			op = OP_LT;
			break;
		case OPR_GRT:
			op = OP_GT;
			break;
		case OPR_CON_MRK:
		case OPR_CON_SEP:
			op = OP_CON;
			break;
		case OPR_AND:
			op = OP_AND;
			break;
		case OPR_OR:
			op = OP_OR;
			break;
	}
	return op;
}

int op_to_precedence(enum Operation op) {
	int p = 0; // Precedence
	
	switch (op) {
		case OP_INV:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_NOT:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_EXP:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_MUL:
		case OP_DIV:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_ADD:
		case OP_SUB:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_CAT:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_LT:
		case OP_GT:
		case OP_LTE:
		case OP_GTE:
		case OP_EQU:
		case OP_NEQ:
		case OP_SEQU:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_AND:
		case OP_OR:
			return p;
	}
	++p;
	
	switch (op) {
		case OP_CON:
			return p;
	}
	
	return -1;
}

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

struct Primitive primitive_get(struct Token *token) {
	struct Primitive value;
	
	switch (token->type) {
		case TOK_NUMBER:
			value.type = PRI_NUMBER;
			value.number = token->number;
			break;
		case TOK_STRING:
			value.type = PRI_STRING;
			value.string = malloc(token->data_len + 1);
			strncpy(value.string, token->data, token->data_len);
			break;
		case TOK_BOOL:
			// FIXME: Parse booleans
			break;
	}
	
	return value;
}

bool kwd_is_declarator(enum Keyword kwd) {
	for (size_t i = 0; i < lenof(KWDSET_DECLARATOR); ++i) {
		if (kwd == KWDSET_DECLARATOR[i]) return true;
	}
	return false;
}

struct Expression expression_get(struct Token *tokens, size_t count) {
	struct Expression expression = {.op = OP_NOP};
	
	// Calculate the number of actual tokens (anything not a whitespace)
	size_t actual_count = 0;
	struct Token *actual_tokens = tokens;
	for (size_t i = 0; i < count; ++i) if (tokens[i].type != TOK_WHITESPACE) ++actual_count;
	if (actual_count == count) goto skip_strip;
	
	// Allocate a new array with only actual tokens
	actual_tokens = malloc(sizeof(struct Token) * actual_count);
	if (!actual_tokens) raise_mem("collecting actual tokens");
	size_t actual_i = 0;
	for (size_t i = 0; i < count; ++i) if (tokens[i].type != TOK_WHITESPACE) actual_tokens[actual_i++] = tokens[i];
	skip_strip:;
	
	enum Precedence precedence = PRE__START;
	bool success;
	do {
		success = expression_parse(actual_tokens, actual_count, --precedence, &expression);
		if (expression.op == OP_ERR) raise_error("Unable to parse expression", false);
	} while (!success);
	
	return expression;
}

bool expression_parse(struct Token *token, size_t count, enum Precedence precedence, struct Expression *expression) {
	static char *err_mem_ctx = "parsing expression";
	
	size_t operand_count = 2;
	if (count == 1) {
		expression->op = OP_NOP;
		
		struct Operand *term = malloc(sizeof(struct Operand));
		if (!term) raise_mem(err_mem_ctx);
		term->type = OPE_PRIMITIVE;
		
		term->value = malloc(sizeof(struct Primitive));
		if (!term->value) raise_mem(err_mem_ctx);
		*term->value = primitive_get(token);
		
		expression->operands = term;
	} else {
		struct Token *op_token;
		switch (precedence) {
			case PRE_INV:
				if (token->type != TOK_OPERATOR || token->op_info.sym != OPR_SUB) return false;
				expression->op = OP_INV;
				expression->operands = expression_alloc_operands(operand_count = 1);
				*expression->operands[0].expression = expression_get(token + 1, count - 1);
				break;
			case PRE_NEG:
				if (token->type != TOK_WORD || token->keyword != KWD_NOT) return false;
				expression->op = OP_NOT;
				expression->operands = expression_alloc_operands(operand_count = 1);
				*expression->operands[0].expression = expression_get(token + 1, count - 1);
				break;
			case PRE_EXP:
				op_token = expression_parse_infix_binary(token, count, (enum Operator []){OPR_EXP}, 1, false, expression);
				if (!op_token) return false;
				break;
			case PRE_MUL_DIV:
				op_token = expression_parse_infix_binary(token, count, (enum Operator []){OPR_MUL, OPR_DIV}, 2, true, expression);
				if (!op_token) return false;
				break;
			case PRE_ADD_SUB:
				op_token = expression_parse_infix_binary(token, count, (enum Operator []){OPR_ADD, OPR_SUB}, 2, true, expression);
				if (!op_token) return false;
				break;
			case PRE_CAT:
				op_token = expression_parse_infix_binary(token, count, (enum Operator []){OPR_CAT}, 1, true, expression);
				if (!op_token) return false;
				break;
			case PRE_COMP:
				op_token = expression_parse_comp(token, count, expression);
				if (!op_token) return false;
				break;
			case PRE_CONJ:
				op_token = expression_parse_infix_binary(token, count, (enum Operator []){OPR_AND, OPR_OR}, 2, true, expression);
				if (!op_token) return false;
				break;
			case PRE_ASS:
				op_token = expression_parse_assign(token, count, expression);
				if (!op_token) return false;
				break;
			default:
				expression->op = OP_ERR;
				return false;
		}
		
		// Flatten expression operands
		for (size_t i = 0; i < operand_count; ++i) {
			if (expression->operands[i].expression->op == OP_NOP) {
				struct Expression *wrapped_expression = expression->operands[i].expression;
				expression->operands[i] = wrapped_expression->operands[0];
				free(wrapped_expression);
			}
		}
	}
	
	return true;
}

struct Token *expression_parse_infix_binary(struct Token *tokens, size_t count, enum Operator opr_list[], size_t opr_count, bool left, struct Expression *expression) {
	if (count < 3) return false;
	struct Token *op_token = find_token_by_opr(tokens, count, opr_list, opr_count, left);
	if (!op_token) return NULL;
	expression->op = opr_to_op(op_token->op_info.sym);
	expression->operands = expression_alloc_operands(2);
	*expression->operands[0].expression = expression_get(tokens, op_token - tokens);
	*expression->operands[1].expression = expression_get(op_token + 1, count - (op_token - tokens) - 1);
	return op_token;
}

struct Token *expression_parse_comp(struct Token *tokens, size_t count, struct Expression *expression) {
	if (count < 3) return false;
	struct Token *op_token = find_token_by_opr(tokens, count, (enum Operator []){OPR_GRT, OPR_LES, OPR_EQU}, 3, true);
	if (!op_token) return false;
	if (op_token == tokens || op_token == &tokens[count - 1]) return false;
	
	bool dual_token = op_token[1].type == TOK_OPERATOR;
	if (dual_token) {
		// Dual-token operator
		dual_token = true;
		if (op_token[1].op_info.sym == OPR_EQU) {
			switch (op_token->op_info.sym) {
				case OPR_EQU:
					// Strictly equal to
					expression->op = OP_SEQU;
					break;
				case OPR_GRT:
					// Greater than or equal to
					expression->op = OP_GTE;
					break;
				case OPR_LES:
					// Lesser than or equal to
					expression->op = OP_LTE;
					break;
				default:
					return NULL;
			}
		} else if (op_token->op_info.sym == OPR_LES && op_token[1].op_info.sym == OPR_GRT) {
			// Inequality
			expression->op = OP_NEQ;
		} else return NULL;
		
		expression->operands = expression_alloc_operands(2);
		*expression->operands[0].expression = expression_get(tokens, op_token - tokens);
		*expression->operands[1].expression = expression_get(op_token + 2, count - ((op_token + 1) - tokens) - 1);
	} else {
		switch (op_token->op_info.sym) {
			case OPR_EQU:
				// Equal to
				expression->op = OP_SEQU;
				break;
			case OPR_GRT:
				// Greater than
				expression->op = OP_GT;
				break;
			case OPR_LES:
				// Lesser than
				expression->op = OP_LT;
				break;
			default:
				return NULL;
		}
	}
	
	expression->operands = expression_alloc_operands(2);
	*expression->operands[0].expression = expression_get(tokens, op_token - tokens);
	if (dual_token) op_token += 1;
	*expression->operands[1].expression = expression_get(op_token + 1, count - (op_token - tokens) - 1);
	
	return op_token;
}

struct Token *expression_parse_assign(struct Token *tokens, size_t count, struct Expression *expression) {
	struct Token *op_token = expression_parse_infix_binary(tokens, count, &(enum Operator){OPR_EQU}, 1, false, expression);
	
	if (op_token->op_info.equal_op != OP_EQU) {
		// Modify the expression to add the pre-assignment operation
		struct Operand original = expression->operands[1];
		
		expression->operands[1].type = OPE_EXPRESSION;
		struct Expression *pre_exp = malloc(sizeof(struct Expression));
		if (!pre_exp) raise_mem("parsing assignment expression");
		expression->operands[1].expression = pre_exp;
		
		pre_exp->op = op_token->op_info.equal_op;
		pre_exp->operands = expression_alloc_operands(2);
		pre_exp->operands[0] = expression->operands[0];
		pre_exp->operands[1] = original;
	}
	
	return op_token;
}

struct Operand *expression_alloc_operands(size_t count) {
	static char *err_mem_ctx = "adding operands to an operation";
	
	// Allocate space for operand array
	struct Operand *operands = malloc(sizeof(struct Operand) * count);
	if (!operands) raise_mem(err_mem_ctx);
	
	// Initialize each element with an empty expression
	for (size_t i = 0; i < count; ++i) {
		operands[i].type = OPE_EXPRESSION;
		operands[i].expression = malloc(sizeof(struct Expression));
		if (!operands[i].expression) raise_mem(err_mem_ctx);
	}
	return operands;
}

struct Token *find_token_by_opr(struct Token *tokens, size_t count, enum Operator opr_list[], size_t opr_count, bool left) {
	size_t open_brackets = 0;
	size_t i = left ? 0 : count - 1;
	while (true) {
		if (tokens[i].type == TOK_BRACKET && tokens[i].data[0] == '(') {
			++open_brackets;
			goto next;
		}
		if (open_brackets) {
			if (tokens[i].type == TOK_BRACKET && tokens[i].data[0] == ')') --open_brackets;
			goto next;
		}
		if (tokens[i].type == TOK_OPERATOR) {
			for (size_t opr = 0; opr < opr_count; ++opr) if (tokens[i].op_info.sym == opr_list[opr]) return tokens + i;
		}
		
		// Iterate
		next: if (left) {if (i == count) break; ++i;} else {if (i == 0) break; --i;}
	}
	return NULL;
}

noreturn void raise_error(char *msg, bool free_msg) {
	if (parse_error.free_msg && parse_error.msg) free(parse_error.msg);
	
	parse_error.msg = msg;
	parse_error.free_msg = free_msg;
	
	longjmp(parse_error.jump, true);
}

noreturn void raise_error_fmt(char *def, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	
	char *msg;
	int result = vasprintf(&msg, fmt, args);
	if (result == -1) msg = def;
	
	va_end(args);
	
	raise_error(msg, result != -1);
}

noreturn void raise_mem(char *context) {
	char *msg = "Failed to allocate memory";
	if (context) {
		raise_error_fmt(msg, "%s when %s", msg, context);
	} else {
		raise_error(msg, false);
	};
}

noreturn void raise_unexpected_token(char *expected, struct Token *got_token) {
	char *def_msg = "Unexpected token encountered!";
	
	// Find line number and position
	size_t line_num = 0;
	struct Token *first_token = got_token;
	while (first_token[-1].type != TOK_EOF) --first_token;
	char *code = first_token->data;
	char *line_start = code;
	do {
		if (*code == '\n') {
			++line_num;
			line_start = code + 1;
		}
	} while (++code != got_token->data);
	
	if (expected) {
		raise_error_fmt(def_msg,
			"Unexpected token encountered at %zu:%tu! Was expecting %s but instead got '%.*s'",
			line_num,
			got_token->data - line_start,
			expected,
			(int) got_token->data_len,
			got_token->data
		);
	} else {
		raise_error_fmt(def_msg,
			"Unexpected token encountered at %zu:%tu! Was not expecting '%.*s'",
			line_num,
			got_token->data - line_start,
			(int) got_token->data_len,
			got_token->data
		);
	}
}
