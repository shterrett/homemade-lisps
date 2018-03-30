#include <stdio.h>

void printHello(int times);

int main(int argc, char** argv) {
  for (int i = 0; i < 5; i++) {
    puts("Hello For");
  }

  int counter = 0;
  while (counter < 5) {
    puts("Hello While");
    counter++;
  }

  printHello(7);

  return 0;
}

void printHello(int times) {
  for (int i = 0; i < times; i++) {
    puts("Hello Function");
  }
}
