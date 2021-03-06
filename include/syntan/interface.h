// Interface.h
#ifndef INTERFACE_H
#define INTERFACE_H

#include "lexical.h"

Symbol* next_symbol(); // Allocate and return a symbol
Token* next_token(); // Allocate and return a token

void symbol_free(Symbol *s);
void token_free(Token *t);

Symbol* temp_add();
Symbol* block_add();

void asm_write(char *input);
void asm_f_append(char *file);
void asm_data_head();
void asm_bss();
void asm_io_tail();
void asm_exit();
void procedure_add(char *proc);
void asm_append_procedures();

void blockfile_open(char *file);
void blockfile_write(char *input);
void blockfile_close();

void parser_init(char *tokens, char *symbols, char *assembly);
void parser_release();

#endif // INTERFACE_H