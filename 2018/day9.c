#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MAX(a,b) (((a)>(b))?(a):(b))

uint64_t run(int64_t players, int64_t marbles) {
  size_t memsize = marbles * sizeof(uint32_t);
    
  uint32_t *prevs = malloc(memsize);
  uint32_t *nexts = malloc(memsize);
  int64_t *scores = calloc(1024, sizeof(int64_t));

  uint32_t n, l;
  uint32_t c = 0;
  prevs[c] = c;
  nexts[c] = c;

  int64_t max = 0;
  int64_t x;
  int64_t player = 0;

  for(int64_t i=1; i < marbles+1; i++) {
    if(i % 23 == 0) {
      for(x=0;x<7;x++) c = prevs[c];
      scores[player] += c + i;
      max = MAX(scores[player], max);
      nexts[prevs[c]] = nexts[c];
      prevs[nexts[c]] = prevs[c];
      l = c;
      c = nexts[c];
    } else {
      c = nexts[c];
      n = i;
      prevs[n] = c;
      nexts[n] = nexts[c];

      prevs[nexts[c]] = n;
      nexts[c] = n;
      c = n;
    }
    player = (player+1) % (players);
  }

  free(scores);
  free(prevs);
  free(nexts);
  
  return max;
}

int main() {

  int64_t players, marbles;
  scanf("%lu players; last marble is worth %lu points", &players, &marbles);

  printf("%lu\n", run(players, marbles));
  printf("%lu\n", run(players, marbles * 100));
  
  return 0;
}
