#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "mpc.h"

#ifdef _WIN32

static char buffer[2048];

char *readline(char *prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char *cpy = malloc(strlen(buffer) + 1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy) - 1] = '\0';
  return cpy;
}

void add_history(char* unused) {}

#else
#include <editline/readline.h>
#endif

int main(int argc, char** argv) {

  /* Create Parsers */
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Operator = mpc_new("operator");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  /* Define Parsers */
  mpca_lang(MPCA_LANG_DEFAULT,
      "number : /-?[0-9]+/ ;"
      "operator : '+' | '-' | '*' | '/' ;"
      "expr : <number> | '(' <operator> <expr>+ ')' ;"
      "lispy : /^/ <operator> <expr>+ /$/ ;",
      Number, Operator, Expr, Lispy);

  puts("Lispy version 0.0.0.1");
  puts("Press ctrl+c to exit");

  while (1) {
    char *input = readline("lispy> ");
    add_history(input);

    if (strncmp(input, "exit", 4) == 0) {
      mpc_cleanup(4, Number, Operator, Expr, Lispy);
      return 0;
    }

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      mpc_ast_print(r.output);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(4, Number, Operator, Expr, Lispy);
  return 0;
}
