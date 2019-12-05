#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int dist(int x1, int y1, int x2, int y2) { return abs(x1 - x2) + abs(y1 - y2); }

int main() {
  int *xs = calloc(4096, sizeof(int));
  int *ys = calloc(4096, sizeof(int));
  int *sums = calloc(4096, sizeof(int));
  int *x = xs, *y = ys;
  int len = 0;
  while(scanf("%d, %d\n", x, y) > 0) {
    x++;
    y++;
    len++;
  }

  int part2 = 0; // part 2
  for(int xi=0; xi<512;xi++) {
    for(int yi=0; yi<512;yi++) {
      int closest = 20000000;
      int closest2 = -5;
      int closest_i = -5;
      int total_dist = 0; // part 2
      for(int i=0; i<len;i++) {
	int d = dist(xi, yi, xs[i], ys[i]);
	total_dist += d; // part 2
	if(d <= closest) {
	  closest2 = closest;
	  closest = d;
	  closest_i = i;
	} 
      }
      if(total_dist < 10000) part2++; // part 2
      if(xi == 0 || yi == 0 || xi == 511 || yi == 511) sums[closest_i] = 20000000;
      if(!(closest == closest2)) sums[closest_i]++;
    }
  }

  int max = 0;
  for(int mi=0; mi<len; mi++) {
    if(sums[mi] >= 20000000) continue;
    if(sums[mi] > max) { max = sums[mi]; }
  }
  printf("%d\n", max);
  printf("%d\n", part2); // part 2

  return 0;
}
