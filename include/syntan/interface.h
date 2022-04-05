// Interface.h
#ifndef INTERFACE_H
#define INTERFACE_H

#include "lexical.h"

Symbol* next_symbol(); // Allocate and return a symbol
Token* next_token(); // Allocate and return a token

void symbol_free(Symbol *s);
void token_free(Token *t);

Symbol* temp_add();

void parser_init(char *tokens, char *symbols);
void parser_release();

#endif // INTERFACE_H