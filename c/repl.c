#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "mpc.h"

#define LASSERT(args, cond, fmt, ...)\
  if (!(cond)) { \
    lval *err = lval_err(fmt, ##__VA_ARGS__); \
    lval_del(args); \
    return err; \
  }

#define LASSERT_NUM(sym, args, expected)\
  if (!(args->count == expected)) { \
      lval *err = lval_err("Expected %d to equal %d in %s", args->count, expected, sym); \
      lval_del(args); \
      return err; \
  }

#define LASSERT_TYPE(sym, args, child, expected) \
  if (!(args->cell[child]->type == expected)) { \
    lval *err = lval_err("Expected type of %s to be %s", ltype_name(args->type), ltype_name(expected)); \
    lval_del(args); \
    return err; \
  }

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

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

typedef lval*(*lbuiltin)(lenv*, lval*);

enum { LVAL_NUM,
       LVAL_ERR,
       LVAL_SYM,
       LVAL_FUN,
       LVAL_SEXPR,
       LVAL_QEXPR };

struct lval {
  int type;

  /* Basic */
  long num;
  char *err;
  char *sym;

  /* Function */
  lbuiltin builtin;
  lenv *env;
  lval *formals;
  lval *body;

  /* Expression */
  int count;
  lval **cell;
};

struct lenv {
  int count;
  char **syms;
  lval **vals;
  lenv *parent;
};

lval *lval_read(mpc_ast_t *t);
lval *lval_num(long x);
lval *lval_err(char *fmt, ...);
lval *lval_sym(char *s);
lval *lval_fun(lbuiltin func);
lval *lval_qexpr(void);
lval *lval_sexpr(void);
lval *lval_lambda(lval *formals, lval *body);
lval *lval_add(lval *v, lval *x);
void lval_print(lval *v);
void lval_println(lval *v);
char *lval_expr_show(lval *v, char *open, char *close);
char *lval_show(lval *v);
void lval_del(lval *v);
lval *lval_eval(lenv *e, lval *v);
lval *lval_eval_sexpr(lenv *e, lval *v);
lenv *lenv_new(void);
lval *lval_call(lenv *e, lval *f, lval *a);
void lenv_del(lenv *e);
lenv *lenv_copy(lenv *e);
lval *lenv_get(lenv *e, lval *k);
void lenv_def(lenv *e, lval *k, lval *v);
void lenv_put(lenv *e, lval *k, lval *v);
void  lenv_add_builtins(lenv *e);
char *ltype_name(int t);

int main(int argc, char** argv) {

  /* Create Parsers */
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Symbol = mpc_new("symbol");
  mpc_parser_t *Sexpr = mpc_new("sexpr");
  mpc_parser_t *Qexpr = mpc_new("qexpr");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  /* Define Parsers */
  mpca_lang(MPCA_LANG_DEFAULT,
      "number : /-?[0-9]+/ ;"
      "symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;"
      "sexpr : '(' <expr>* ')' ;"
      "qexpr : '{' <expr>* '}' ;"
      "expr : <number> | <symbol> | <sexpr> | <qexpr> ;"
      "lispy : /^/ <expr>+ /$/ ;",
      Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

  puts("Lispy version 0.0.0.1");
  puts("Press ctrl+c to exit");

  lenv *env = lenv_new();
  lenv_add_builtins(env);

  while (1) {
    char *input = readline("lispy> ");
    add_history(input);

    if (strncmp(input, "exit", 4) == 0) {
      mpc_cleanup(4, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
      return 0;
    }

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval *x = lval_eval(env, lval_read(r.output));
          lval_println(x);
      lval_del(x);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(4, Number, Symbol, Expr, Lispy);
  return 0;
}

lval *lval_pop(lval *v, int i) {
  lval *x = v->cell[i];
  memmove(&v->cell[i],
      &v->cell[i+1],
      sizeof(lval*) * (v->count - i - 1));

  v->count--;

  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  return x;
}

lval* lval_take(lval *v, int i) {
  lval *x = lval_pop(v, i);
  lval_del(v);
  return x;
}

lval *builtin_head(lenv *e, lval *a) {
  LASSERT(a, a->count == 1,
    "Function 'head' passed too many arguments!\n"
    "Got %i Expected %i",
    a->count, 1
    );

  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'head' expected %s but got %s",
    ltype_name(LVAL_QEXPR),
    ltype_name(a->cell[0]->type)
  );

  LASSERT(a, a->cell[0]->count > 0,
    "Function 'head' passed {}!");

  lval *v = lval_take(a, 0);
  while (v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval *builtin_tail(lenv *e, lval *a) {
  LASSERT(a, a->count == 1,
    "Function 'tail' passed too many arguments!\n"
    "Got %i Expected %i",
    a->count, 1
    );

  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'tail' expected %s but got %s",
    ltype_name(LVAL_QEXPR),
    ltype_name(a->cell[0]->type)
  );

  LASSERT(a, a->cell[0]->count > 0,
    "Function 'tail' passed {}!");

  lval *v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval *builtin_list(lenv *e, lval *a) {
  a->type = LVAL_QEXPR;
  return a;
}

lval *builtin_eval(lenv *e, lval *a) {
  LASSERT(a, a->count == 1,
    "Function 'eval' passed too many arguments!\n"
    "Got %i Expected %i",
    a->count, 1
    );
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'head' expected %s but got %s",
    ltype_name(LVAL_QEXPR),
    ltype_name(a->cell[0]->type)
  );

  lval *x = lval_take(a, 0);
  x->type = LVAL_SEXPR;
  return lval_eval(e, x);
}

lval *lval_join(lval *x, lval *y) {
  while (y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }

  lval_del(y);
  return x;
}

lval *builtin_join(lenv *e, lval *a) {
  for (int i = 0; i < a->count; i++) {
    LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
      "Function 'join' expected %s but got %s",
      ltype_name(LVAL_QEXPR),
      ltype_name(a->cell[0]->type)
    );
  }

  lval *x = lval_pop(a, 0);

  while (a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }

  lval_del(a);
  return x;
}

lval *builtin_op(lenv *e, lval *a, char *op) {
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != LVAL_NUM) {
      lval_del(a);
      return lval_err("Builtin %s requires number", op);
    }
  }

  lval *x = lval_pop(a, 0);

  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  }

  while (a->count > 0) {
    lval *y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division by zero");
      }
      x->num /= y->num;
    }
    lval_del(y);
  }
  lval_del(a);
  return x;
}

lval *builtin_add(lenv *e, lval *a) {
  return builtin_op(e, a, "+");
}

lval *builtin_sub(lenv *e, lval *a) {
  return builtin_op(e, a, "-");
}

lval *builtin_mul(lenv *e, lval *a) {
  return builtin_op(e, a, "*");
}

lval *builtin_div(lenv *e, lval *a) {
  return builtin_op(e, a, "/");
}

lval *lval_eval(lenv *e, lval *v) {
  if(v->type == LVAL_SYM) {
    lval *x = lenv_get(e, v);
    lval_del(v);
    return x;
  }
  if (v->type == LVAL_SEXPR) { return lval_eval_sexpr(e, v); }
  return v;
}

lval *builtin_var(lenv *e, lval *a, char *func) {
  LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

  lval *syms = a->cell[0];
  for (int i = 0; i < syms->count; i++) {
    LASSERT(a, syms->cell[i]->type == LVAL_SYM,
        "Function 'def' cannot define non-symbol %s in %s",
        lval_show(syms->cell[i]),
        lval_show(a));
  }

  LASSERT(a, syms->count == a->count-1,
      "Function def cannot define incorrect number of values to symbols. Expected %d got %d",
      a->count - 1,
      syms->count);

  for (int i = 0; i < syms->count; i++) {
    if (strcmp(func, "def") == 0) {
      lenv_def(e, syms->cell[i], a->cell[i+1]);
    }

    if (strcmp(func, "=") == 0) {
      lenv_put(e, syms->cell[i], a->cell[i+1]);
    }
  }

  lval_del(a);
  return lval_sexpr();
}

lval *builtin_put(lenv *e, lval *a) {
  return builtin_var(e, a, "=");
}

lval *builtin_def(lenv *e, lval *a) {
  return builtin_var(e, a, "def");
}

lval *lval_eval_sexpr(lenv *e, lval *v) {
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }

  for (int i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
  }

  if (v->count == 0) { return v; }

  if (v->count == 1) { return lval_take(v, 0); }

  lval *f = lval_pop(v, 0);
  if (f->type != LVAL_FUN) {
    lval_del(f); lval_del(v);
    return lval_err("First value must be a function, but is %s", ltype_name(f->type));
  }

  lval *result = lval_call(e, f, v);
  lval_del(f);
  return result;
}

lval *builtin_lambda(lenv *e, lval *a) {
  LASSERT_NUM("\\", a, 2);
  LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
  LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

  for (int i = 0; i < a->cell[0]->count; i++) {
    LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
        "Cannot define non-symbol. Got %s, expected %s",
        ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
  }

  lval *formals = lval_pop(a, 0);
  lval *body = lval_pop(a, 0);
  lval_del(a);

  return lval_lambda(formals, body);
}

lval *lval_add(lval *v, lval *x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval *lval_read_num(mpc_ast_t *t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ?
    lval_num(x) : lval_err("%s is not a valid number", t->contents);
}

lval *lval_read(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) { return lval_read_num(t); }
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }

  lval *x = NULL;
  /* root */
  if (strcmp(t->tag, ">") == 0) { x = lval_sexpr(); }
  /* sexpr */
  if (strstr(t->tag, "sexpr")) { x = lval_sexpr(); }
  /* qexpr */
  if (strstr(t->tag, "qexpr")) { x = lval_qexpr(); }

  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
    if (strcmp(t->children[i]->tag, "regex") == 0) { continue; }
    x = lval_add(x, lval_read(t->children[i]));
  }

  return x;
}

lval *lval_num(long x) {
  lval *v = malloc(sizeof(lval));;
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval *lval_err(char *fmt, ...) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  va_list va;
  va_start(va, fmt);
  v->err = malloc(512);
  vsnprintf(v->err, 511, fmt, va);
  v->err = realloc(v->err, strlen(v->err) + 1);
  va_end(va);

  return v;
}

lval *lval_sym(char *s) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval *lval_sexpr(void) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval *lval_qexpr(void) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval *lval_fun(lbuiltin func) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = func;
  return v;
}

lval *lval_lambda(lval *formals, lval *body) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = NULL;
  v->env = lenv_new();
  v->formals = formals;
  v->body = body;

  return v;
}

lval *lval_copy(lval *v) {
  lval *x = malloc(sizeof(lval));
  x->type = v->type;

  switch (v->type) {
    case LVAL_FUN:
      if (v->builtin) {
        x->builtin = v->builtin;
      } else {
        x->builtin = NULL;
        x->env = lenv_copy(v->env);
        x->formals = lval_copy(v->formals);
        x->body = lval_copy(v->body);
      }
      break;
    case LVAL_NUM: x->num = v->num; break;
    case LVAL_ERR:
      x->err = malloc(strlen(v->err) + 1);
      strcpy(x->err, v->err);
      break;
    case LVAL_SYM:
      x->sym = malloc(strlen(v->sym) + 1);
      strcpy(x->sym, v->sym);
      break;

    case LVAL_SEXPR:
    case LVAL_QEXPR:
      x->count = v->count;
      x->cell = malloc(sizeof(lval*) * x->count);
      for (int i = 0; i < x->count; i++) {
        x->cell[i] = lval_copy(v->cell[i]);
      }
      break;
  }

  return x;
}

void lval_del(lval *v) {
  switch (v->type) {
    case LVAL_NUM:
      break;
    case LVAL_ERR:
      free(v->err);
      break;
    case LVAL_SYM:
      free(v->sym);
      break;
    case LVAL_QEXPR:
    case LVAL_SEXPR:
      for (int i = 0; i < v->count; i++) {
        lval_del(v->cell[i]);
      }
      free(v->cell);
      break;
    case LVAL_FUN:
      if (!v->builtin) {
        lenv_del(v->env);
        lval_del(v->formals);
        lval_del(v->body);
      }
      break;
  }

  free(v);
}

lval *lval_call(lenv *e, lval *f, lval *a) {
  if (f->builtin) { return f->builtin(e, a); }

  int given = a->count;
  int total = f->formals->count;

  while (a->count) {
    if (f->formals->count == 0) {
      lval_del(a); return lval_err(
          "Function passed too many arguments "
          "Got %i, expected %i", given, total);
    }

    lval *sym = lval_pop(f->formals, 0);
    lval *val = lval_pop(a, 0);
    lenv_put(f->env, sym, val);
    lval_del(sym);
    lval_del(val);
  }

  lval_del(a);

  if (f->formals->count == 0) {
    f->env->parent = e;
    return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
  } else {
    return lval_copy(f);
  }
}

lenv *lenv_new(void) {
  lenv *e = malloc(sizeof(lenv));
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  e->parent = NULL;
  return e;
}

void lenv_del(lenv *e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }

  free(e->syms);
  free(e->vals);
  free(e);
}

lenv *lenv_copy(lenv *e) {
  lenv *n = malloc(sizeof(lenv));
  n->parent = e->parent;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i] + 1));
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }

  return n;
}

lval *lenv_get(lenv *e, lval *k) {
  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      return lval_copy(e->vals[i]);
    }
  }

  if (e->parent) {
    return lenv_get(e->parent, k);
  } else {
    return lval_err("Unbound symbol '%s'", k->sym);
  }
}

void lenv_def(lenv *e, lval *k, lval *v) {
  while (e->parent) { e = e->parent; }
  lenv_put(e, k, v);
}

void lenv_put(lenv *e, lval *k, lval *v) {
  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      lval_del(e->vals[i]);
      e->vals[i] = lval_copy(v);
      return;
    }
  }

  e->count++;
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  e->vals[e->count-1] = lval_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym) + 1);
  strcpy(e->syms[e->count-1], k->sym);
}

void lenv_add_builtin(lenv *e, char *name, lbuiltin func) {
  lval *k = lval_sym(name);
  lval *v = lval_fun(func);
  lenv_put(e, k, v);
  lval_del(k);
  lval_del(v);
}

void lenv_add_builtins(lenv *e) {
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "=", builtin_put);
  lenv_add_builtin(e, "\\", builtin_lambda);

  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
}
char *lval_show(lval *v) {
  char *show = malloc(512);
  switch (v->type) {
    case LVAL_NUM: snprintf(show, 511, "%li", v->num); break;
    case LVAL_ERR: snprintf(show, 511, "%s", v->err); break;
    case LVAL_SYM: snprintf(show, 511, "%s", v->sym); break;
    case LVAL_SEXPR: lval_expr_show(v, "(", ")"); break;
    case LVAL_QEXPR: lval_expr_show(v, "{", "}"); break;
    case LVAL_FUN: snprintf(show, 511, "%s", "<function>"); break;
  }

  return show;
}

char *lval_expr_show(lval *v, char *open, char *close) {
  char *elems[v->count];

  for (int i = 0; i < v->count; i++) {
    elems[i] = lval_show(v->cell[i]);
  }

  int total_len = 2; // open and close brackets
  for (int i = 0; i < v->count; i++) {
    total_len += strlen(elems[i]);
    total_len += 1; // space or null
  }

  char *show = malloc(total_len * sizeof(char));
  strcat(show, open);
  for (int i = 0; i < v->count; i++) {
    strcat(show, elems[i]);
    if (i < v->count - 1) {
      strcat(show, " ");
    }
  }
  strcat(show, close);

  return show;
}


void lval_expr_print(lval *v, char open, char close) {
  putchar(open);
  for (int i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);

    if (i != (v->count-1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void lval_print(lval *v) {
  switch (v->type) {
    case LVAL_NUM: printf("%li", v->num); break;
    case LVAL_ERR: printf("%s", v->err); break;
    case LVAL_SYM: printf("%s", v->sym); break;
    case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
    case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
    case LVAL_FUN:
      if (v->builtin) {
        printf("<builtin>");
      } else {
        printf("(\\ ");
        lval_print(v->formals);
        putchar(' ');
        lval_print(v->body); putchar(')');
      }
      break;
  }
}

void lval_println(lval *v) { printf("#=> "); lval_print(v); putchar('\n'); }

char *ltype_name(int t) {
  switch(t) {
    case LVAL_NUM: return "Number";
    case LVAL_ERR: return "Error";
    case LVAL_SYM: return "Symbol";
    case LVAL_FUN: return "Function";
    case LVAL_SEXPR: return "S-Exp";
    case LVAL_QEXPR: return "Q Exp";
    default: return "unknown";
  }
}
