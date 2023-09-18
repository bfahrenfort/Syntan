#include <stdio.h>
#include "syntan/interface.h"
#include "syntan/lexical.h"
#include "HsFFI.h"
#include "stubs/Parser_stub.h"

int main(int argc, char **argv)
{
  if(argc != 3)
  {
    printf("Usage: %s lexemes.lex symbols.sym\n"
          "\tlexemes.lex: path to tokens/lexemes from Lexicalli output\n"
          "\tsymbols.sym: path to symbol table from Lexicalli output\n", argv[0]);
    return 1;
  }
  
  hs_init(&argc, &argv);
  parser_init(argv[1], argv[2]);
  int32_t ret = runParser(); // Call Haskell
  parser_release();
  hs_exit();
  
  printf("Syntan: Exit\n");

  return ret;
}