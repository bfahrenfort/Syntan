#include <stdio.h>
#include "syntan/interface.h"
#include "syntan/lexical.h"
#include "HsFFI.h"
#include "stubs/Parser_stub.h"

int main(int argc, char **argv)
{
  // Look at tokens and symbol table to generate quads
  hs_init(&argc, &argv);
  parser_init("pg63_jz.lex", "pg63_jz.sym");
  runParser(); // Call Haskell
  parser_release();
  
  // Generate assembly

  hs_exit();
  
}