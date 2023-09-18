#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "syntan/lexical.h"
#include "syntan/interface.h"

FILE *token_file, *symbol_file, *asm_file;
int instr_ptr = 0;
int num_temps = 0;
int num_blocks = 0;

Symbol* next_symbol()
{
  Symbol *symbol = malloc(sizeof(Symbol));
  symbol->name = malloc(128);
  symbol->value = malloc(128);
  
  if(!feof(symbol_file))
  {
    fscanf(symbol_file, 
              "%s %d %s %d %d", 
              symbol->name, 
              (int*) &symbol->sym_class, 
              symbol->value, 
              &symbol->address, 
              (int*) &symbol->segment);
  }
  else
    symbol->sym_class = SINVALID; // Let Haskell know we're out of symbols
    
  return symbol;
}

Token* next_token()
{
  Token *token = malloc(sizeof(Token));
  token->name = malloc(128);
  if(!feof(token_file))
    fscanf(token_file, "%s %d", token->name, (int*) &token->tok_class);
  else
    token->tok_class = TINVALID;
  return token;
}

void symbol_free(Symbol *s)
{
  if(s != NULL)
  {
    free(s->name);
    free(s->value);
    free(s);
  }
}

void token_free(Token *t)
{
  if(t != NULL)
  {
    free(t->name);
    free(t);
  }
}

Symbol* temp_add()
{
  Symbol *ret = malloc(sizeof(Symbol));
  char temp_num[12];
  ret->name = malloc(128);
  ret->value = malloc(128);
  
  // Assign name (T1, T2, ...)
  ++num_temps;
  strcpy(ret->name, "T");
  sprintf(temp_num, "%d", num_temps);
  strcat(ret->name, temp_num);

  // Default value
  strcpy(ret->value, "-");
  // TODO: write temp to symbol table file
  return ret;
}

Symbol* block_add()
{
  Symbol *ret = malloc(sizeof(Symbol));
  char block_num[12];
  ret->name = malloc(128);
  ret->value = malloc(128);
  
  // Assign name (B1, B2, ...)
  ++num_blocks;
  strcpy(ret->name, "B");
  sprintf(block_num, "%d", num_blocks);
  strcat(ret->name, block_num);

  // Default value
  strcpy(ret->value, "-");
  // TODO: write block to symbol table file
  return ret;
}

void asm_write(char *input)
{
  fputs(input, asm_file);
}

void procedure_block_write(Symbol *proc)
{

}

void parser_init(char *tokens, char *symbols)
{
  token_file = fopen(tokens, "r");
  symbol_file = fopen(symbols, "r");

  char c;
  do
  {
    c = fgetc(token_file);
  } while(c != '\n'); // Skip first line

  c = '\0';
  do
  {
    c = fgetc(symbol_file);
  } while(c != '\n'); // Skip first line
}

void parser_release()
{
  fclose(token_file);
  fclose(symbol_file);
}