#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "util.c"

void part1(int list[], int length) {
  int i;
  int32_t freq = 0;
  for (i=0; i<length; i++) {
    freq += list[i];
  }
   printf("%i\n", freq);
}

void part2(int list[], int length) {
  int i;
  int freq = 0;
  btree *tree = 0;
  btree_insert(&tree, 0);
  while(1) {
    for (i=0; i<length; i++) {
      freq += list[i];
      if(btree_insert(&tree, freq) != 1) {
	      printf("%i\n", freq);
	      return;
      }
    }
  }
}

int main() {
  char buff[100];
  int list[2048];
  int count = 0;
  while(readline(stdin, buff, 100) > 0) {
    list[count++] = atoi(buff);
  }
  part1(list, count);
  part2(list, count);
  
  return 0;
}
