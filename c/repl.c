#include<stdio.h>
#include<stdlib.h>
#include<string.h>

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
  puts("Lispy version 0.0.0.1");
  puts("Press ctrl+c to exit");

  while (1) {
    char *input = readline("lispy> ");
    add_history(input);

    if (strncmp(input, "exit", 4) == 0) {
      return 0;
    }

    printf("#=> %s\n", input);

    free(input);
  }
  return 0;
}
