#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

typedef struct req {
    int16_t l;
    int16_t t;
    int16_t r;
    int16_t b;
    int16_t id;
} req;

uint64_t rect_area(req a) {
    return (a.r - a.l) * (a.b - a.t);
}

int rect_intersects(req a, req b) {
    return a.t < b.b && b.t < a.b && a.l < b.r && b.l < a.r;
}

req intersection_rect(req a, req b) {
    return (req){ MAX(a.l, b.l), MAX(a.t, b.t), MIN(a.r, b.r), MIN(a.b, b.b), 0 };
}

void grid_set_bit(uint64_t *g, size_t gw, size_t x, size_t y) {
    uint8_t xy_small = (y & 0b111)<<3 | (x & 0b111);
    uint32_t xy_big = ((y>>3) * gw) + ((x>>3) % gw);
    g[xy_big] |= (uint64_t)0b1<<xy_small;
}

uint64_t grid_popcount(uint64_t *g, size_t gw, size_t gh) {
    uint64_t sum = 0;
    for(int i=0;i<(gw*gh);i++) sum += __builtin_popcountll(g[i]);
    return sum;
}

int main() {
  req cur, list[2048];
  int length = 0;
  while(1) {
    cur = (req){0};
    if(scanf("#%hd @ %hd,%hd: %hdx%hd\n", &cur.id, &cur.l, &cur.t, &cur.r, &cur.b) <= 0) break;
    cur.r += cur.l;
    cur.b += cur.t;
    list[count++] = cur;
  }
  part1(list, count);

  int i, n;
  req cur;
  int *intersects = calloc(length, sizeof(int));
  req *overlaps = calloc(2048*2, sizeof(req));
  int ocount = 0;

  // calculate intersection rectancles
  for (i=0; i<length; i++)
    for(n=0; n<i; n++)
      if(rect_intersects(list[i], list[n])) {
	overlaps[ocount++] = intersection_rect(list[i], list[n]);
	intersects[i] = 1;
	intersects[n] = 1;
      }

  uint64_t gsize = 1024/8;
  uint64_t *grid = calloc(gsize * gsize, sizeof(uint64_t));
  for(i=0;i<ocount;i++) {
    cur = overlaps[i];
    for(int x=cur.l;x<cur.r;x++)
      for(int y=cur.t;y<cur.b;y++)
	grid_set_bit(grid, gsize, x, y);
  }
  // 1. insert in batches, n^2 blocks can be inserted as a single rect at once
  // 2. maintaining the sum at every insertion

  // part 1
  printf("%lu\n", grid_popcount(grid, gsize, gsize));
  // part 2
  for(i=0;i<length;i++) { if(intersects[i] == 0) printf("%d\n", list[i].id); }

  return 0;
}
