#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

typedef int64_t i64;
void max_bounds(int *ps, int *vs, int length, i64 t, i64 *out) {
  i64 xmin = INT64_MAX;
  i64 ymin = INT64_MAX;
  i64 xmax = 0;
  i64 ymax = 0;
  i64 x, y;
  for(int i = 0;i<length; i+=2) {
    x = ps[i] + (vs[i] * t);
    y = ps[i+1] + (vs[i+1] * t);
    xmin = MIN(x, xmin);
    xmax = MAX(x, xmax);
    ymin = MIN(y, ymin);
    ymax = MAX(y, ymax);
  }
  out[0] = xmin;
  out[1] = ymin;
  out[2] = xmax;
  out[3] = ymax;
}
i64 calc_area(int *ps, int *vs, int length, i64 t) {
  i64 bounds[4];
  max_bounds(ps, vs, length, t, bounds);
  return (bounds[2] - bounds[0]) * (bounds[3] - bounds[1]);
}

int main() {
  // position=<-42417,  32097> velocity=< 4, -3>
  int px, py, vx, vy;

  int *ps = calloc(2048, sizeof(int) * 2);
  int *vs = calloc(2048, sizeof(int) * 2);
  int i = 0;
  while(scanf("position=<%d, %d> velocity=<%d, %d>\n", &px, &py, &vx, &vy) > 0) {
    ps[i] = px;   vs[i] = vx;
    ps[i+1] = py; vs[i+1] = vy;
    i+=2;
  }
  int length = i-1;

  i64 dir = 1, step_size = 1;
  i64 iters = 0, max_iters = 1000;
  i64 t = 0;
  i64 best = INT64_MAX;
  i64 best_time = INT64_MAX;
  i64 prev, cur = calc_area(ps, vs, length, 0);
  i64 step_mul = 2;
  while(step_size >= 1 && iters++ < max_iters) {
    if(cur < best) { best = cur; best_time = t; }
    t += step_size * dir;
    prev = cur;
    cur = calc_area(ps, vs, length, t);
    cur = (t < 0 || cur < 1) ? INT64_MAX : cur;
    if(cur < prev) step_size *= step_mul;
    else if(cur > prev) {
      step_mul = 1;
      dir *= -1;
      step_size /= 2;
    }
    //printf("%3ld, %8ld, %12ld\n", iters, t, cur);
  }
  t = best_time; cur = best;
  // printf("%3ld, %8ld, %12ld\n", iters, t, cur);
  if (iters >= max_iters) exit(0);

  int *psout = calloc(2048, sizeof(i64) * 2);
  for(i=0; i<length; i+=2) {
    psout[i] = ps[i] + (vs[i] * t);
    psout[i+1] = ps[i+1] + (vs[i+1] * t);
  }
  i64 bounds[4];
  max_bounds(ps, vs, length, t, bounds);
  int width = bounds[2]-bounds[0]+1;
  int height = bounds[3]-bounds[1]+1;
  int **grid = calloc(height, sizeof(int**));
  for(i=0;i<height;i++) grid[i] = calloc(width, sizeof(int));

  for(i=0; i<length; i+=2) 
    grid[psout[i+1]-bounds[1]][psout[i]-bounds[0]] = 1;
    
  for(int yi=0; yi<height; yi++) {
    for(int xi=0; xi<width; xi++) 
      printf("%c", grid[yi][xi] ? '#' : ' ');
    printf("\n");
  }
  printf("%ld\n", t);
}
