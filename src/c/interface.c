#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "syntan/lexical.h"
#include "syntan/interface.h"

FILE *token_file, *symbol_file, *asm_file, *block_file;
int instr_ptr = 0;
int num_temps = 0;
int num_blocks = 0;

// Memory leak prevention
typedef struct FreeList_t
{
  Symbol *to_be_freed;
  struct FreeList_t *next;
} FreeList;
FreeList *free_list = NULL;

typedef struct ProcedureAppend_t
{
  char *proc;
  struct ProcedureAppend_t *next;
} ProcedureAppend;
ProcedureAppend *append_list = NULL;

// Comma-separated C-readable byte value files as char arrays for appending
const char data_head[] = { 
  #include "../../data_head.xxd" 
  }; // IO constants etc
const char bss[] = { 
  #include "../../bss.xxd" 
  }; // 'global _start' etc
const char exit_symbol[] = { 
  #include "../../asm_exit.xxd" 
  }; // 'global _start' etc
const char io_tail[] = { 
  #include "../../io_tail.xxd" 
  }; // GetAnInteger, PrintString, etc

char* format_output(char* input, char *extension)
{
  char *out = malloc(strlen(input) + 5); // length of inputname_inputextension.lex\0
  strcpy(out, input);
  char *dot = strchr(out, '.');
  *dot = '_';
  strcat(out, extension);
  return out;
}

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

void free_list_add(Symbol *addition)
{
  if(free_list == NULL)
  {
    free_list = malloc(sizeof(FreeList));
    free_list->to_be_freed = addition;
    free_list->next == NULL;
  }
  else
  {
    FreeList *pt = free_list;
    while(pt->next != NULL)
      pt = pt->next;
    
    pt->next = malloc(sizeof(FreeList));
    pt->next->to_be_freed = addition;
    pt->next->next = NULL;
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

  ret->sym_class = STEMP;

  // Default value
  strcpy(ret->value, "0");

  free_list_add(ret);
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
  strcpy(ret->value, "?");

  free_list_add(ret);
  return ret;
}

void blockfile_open(char *file)
{
  block_file = fopen(file, "w"); 
}

void blockfile_write(char *input)
{
  if(block_file != NULL)
  {
    fputs(input, block_file);
    fflush(block_file);
  }
  else
    printf("Block file not opened.\n");
}

void blockfile_close()
{
  if(block_file != NULL)
  {
    fclose(block_file);
    block_file = NULL;
  }
  else
    printf("Cannot close blockfile\n");
}

void asm_write(char *input)
{
  fputs(input, asm_file);
  fflush(asm_file);
}

void asm_f_append(char *file)
{
  FILE *inf;
  FILE *of = block_file == NULL ? asm_file : block_file;

  if((inf = fopen(file, "r")) != NULL)
  {
    char buff[128];

    fputc('\n', of);

    int bytes_read;
    while((bytes_read = fread(buff, 1, 128, inf)) > 0)
    {
      fwrite(buff, 1, bytes_read, of);
    }

    fflush(of);

    fclose(inf);
  }
}

void asm_data_head()
{
  fputs(data_head, asm_file);
  fflush(asm_file);
}

void asm_bss()
{
  fputs(bss, asm_file);
  fflush(asm_file);
}

void asm_append_procedures()
{
  ProcedureAppend *pt = append_list;
  ProcedureAppend *dp;
  while(pt != NULL)
  {
      dp = pt;
      pt = pt->next;
      
      char file_name[strlen(dp->proc) + 25]; // syntan_blocks_temp/${proc}.proc\0
      strcpy(file_name, "syntan_blocks_temp/");
      strcat(file_name, dp->proc);
      strcat(file_name, ".proc");
      asm_f_append(file_name);
      free(dp->proc);
      free(dp);
      printf("Appending %s\n", file_name);
  }
}

void procedure_add(char *proc)
{
  if(append_list == NULL)
  {
    append_list = malloc(sizeof(FreeList));
    append_list->proc = malloc(128);
    strcpy(append_list->proc, proc);
    append_list->next = NULL;
  }
  else
  {
    ProcedureAppend *pt = append_list;
    while(pt->next != NULL)
      pt = pt->next;
    
    pt->next = malloc(sizeof(FreeList));
    pt->next->proc = malloc(128);
    strcpy(pt->next->proc, proc);
    pt->next->next = NULL;
  }
}

void asm_exit()
{
  fputs(exit_symbol, asm_file);
  fflush(asm_file);
}

void asm_io_tail()
{
  fputs(io_tail, asm_file);
  fflush(asm_file);
}

void parser_init(char *tokens, char *symbols, char *assembly)
{
  token_file = fopen(tokens, "r");
  symbol_file = fopen(symbols, "r");
  asm_file = fopen(assembly, "w");

  char c;

  if(token_file && symbol_file)
  {
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
  else
  {
    printf("Token or Symbol file not found\n");
    exit(1);
  }
}

void parser_release()
{
  fclose(token_file);
  fclose(symbol_file);
  fclose(asm_file);

  FreeList *pt = free_list;
  FreeList *dp;
  while(pt != NULL)
  {
      dp = pt;
      pt = pt->next;
      symbol_free(dp->to_be_freed);
      free(dp);
  }
}