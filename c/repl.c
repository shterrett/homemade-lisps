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

typedef struct {
  int type;
  long num;
  int err;
} lval;

enum { LVAL_NUM, LVAL_ERR };
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

lval lval_num(long x);
lval lval_err(int x);
void lval_print(lval v);
void lval_println(lval v);

lval eval(mpc_ast_t* t);

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
      lval result = eval(r.output);
      lval_println(result);
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

lval eval_op(lval x, char *op, lval y) {
  if (x.type == LVAL_ERR) { return x; }
  if (y.type == LVAL_ERR) { return y; }

  if (strcmp(op, "+") == 0) { return lval_num(x.num + y.num); }
  if (strcmp(op, "-") == 0) { return lval_num(x.num - y.num); }
  if (strcmp(op, "*") == 0) { return lval_num(x.num * y.num); }
  if (strcmp(op, "/") == 0) {
    if (y.num) {
      return lval_num(x.num / y.num);
    } else {
      return lval_err(LERR_DIV_ZERO);
    }
  }
  return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM);
  }

  /* the operator is always the second child
   * the first expression starts with /^/ (beginning of line)
   * the rest all start with '(' */
  char* op = t->children[1]->contents;

  lval x = eval(t->children[2]);

  int i = 3;
  while (strstr(t->children[i]->tag, "expr")) {
    x = eval_op(x, op, eval(t->children[i]));
    i++;
  }

  return x;
}

lval lval_num(long x) {
  lval v;
  v.type = LVAL_NUM;
  v.num = x;
  return v;
}

lval lval_err(int x) {
  lval v;
  v.type = LVAL_ERR;
  v.err = x;
  return v;
}

void lval_print(lval v) {
  switch (v.type) {
    case LVAL_NUM:
      printf("%li", v.num);
      break;

    case LVAL_ERR:
      switch (v.err) {
        case LERR_DIV_ZERO:
          printf("Error: Division by zero");
          break;
        case LERR_BAD_OP:
          printf("Error: Invaid operator");
          break;
        case LERR_BAD_NUM:
          printf("Error: Invalid number");
          break;
      }
      break;
  }
}
void lval_println(lval v) { printf("#=> "); lval_print(v); putchar('\n'); }
