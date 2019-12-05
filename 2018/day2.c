#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "util.c"

void part1(unsigned char *list[], int length) {
  int i, ci;
  unsigned char *line;
  unsigned char c;
  int twos = 0, threes = 0;
  int two, three;
  uint8_t *counts = calloc(sizeof(uint8_t), 32);
  for(i=0; i < length; i++) {
    ci = 0;
    line = list[i];
    while(1) {
      c = line[ci++];
      if(c == '\0') { break; }
      counts[c-97]++;
    }
        
    two=0;
    three=0;
    for(ci=0; ci < 32; ci++) {
      switch (counts[ci]) {
      case 2: two=1; break;
      case 3: three=1; break;
      }
    }
    twos += two;
    threes += three;
    memset(counts, 0, sizeof(uint8_t)*32);
  }
  printf("%i\n", twos * threes);
}

void part2(unsigned char *list[], int length) {
  int i, n, ci;
  unsigned char *line1, *line2;
  int sim;
  unsigned char sims[32];
  for(i=0; i < length; i++) {
    line1 = list[i];
    for(n=0; n < length; n++) {
      line2 = list[n];
      ci = 0;
      sim = 0;
      while(1) {
	if (line1[ci] == '\0') break;
	if(line1[ci] == line2[ci]) {
	  sims[sim++] = line1[ci];
	}
	ci++;
      }
      if(sim == 25) {
	sims[sim] = '\0';
	printf("%s\n", sims);
	return;
      }
    }
  }
}

int main() {
  size_t cap = 32, len = 0;
  unsigned char **l = malloc(cap*sizeof(*l));
  while (1) {
    unsigned char *s = malloc(27);
    if (scanf("%26s", s) != 1) break;
    if (len >= cap) l = realloc(l, (cap*=2)*sizeof(*l));
    l[len++] = s;
  };

  part1(l, len);
  part2(l, len);
  
  return 0;
}
