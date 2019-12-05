#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int pl(int x, int y, int serial) {
  int rack_id = x + 10;
  int power_level = rack_id * y;
  power_level += serial;
  power_level *= rack_id;
  power_level = (power_level / 100) % 10;
  power_level -= 5;
  return power_level;
}


int main() {
  int input;
  int w = 300;
  int h = 300;

  scanf("%d", &input);

  int *grid = calloc(w * h, sizeof(int));
  for(int x=0;x<w;x++)
    for(int y=0;y<h;y++)
      grid[x*h+y] = pl(x+1, y+1, input);

  int *grid2 = calloc(w * h, sizeof(int));
  int max = 0;
  int max_x = 0;
  int max_y = 0;
  for(int x=0;x<w-2;x++) {
    for(int y=0;y<h-2;y++) {
      for(int x1=x;x1<x+3;x1++)
	for(int y1=y;y1<y+3;y1++)
	  grid2[x*h+y] += grid[x1*h+y1];
      if(grid2[x*h+y] > max) {
	//printf("%d,%d: %d\n", x, y, grid2[x*h+y]);
	max = grid2[x*h+y];
	max_x = x+1;
	max_y = y+1;
      }
    }
  }
  printf("%d,%d\n", max_x, max_y);

  int **cube = calloc(300, sizeof(int*));
  cube[0] = grid; // fill in 1x1 already calculated
  int ssum = 0;
  int max_size = 3;
  int d2, d3, sub, subs;
  // iterate every 2x2...300x300 dimension
  for(int i=1;i<w;i++) {
    cube[i] = calloc(w*h, sizeof(int));
    d2 = ((i+1) % 2) == 0 ? ((i+1)/2) : 0;
    d3 = ((i+1) % 3) == 0 ? ((i+1)/3) : 0;
    //loop over dimensions of new grid tier
    for(int x=0;x<w-i;x++) {
      for(int y=0;y<h-i;y++) {
	ssum = 0;
	//fast path for i divisible by 2
	if(d2) {
	  sub = d2;
	  subs = sub-1;
	  ssum += cube[subs][x*h+y];
	  ssum += cube[subs][(x+sub)*h+y];
	  ssum += cube[subs][x*h+(y+sub)];
	  ssum += cube[subs][(x+sub)*h+(y+sub)];
	  // fast path for i divisible by 3
	} else if(d3) {
	  sub = d3;
	  subs = sub-1;
	  ssum += cube[subs][x*h+y];
	  ssum += cube[subs][(x+sub)*h+y];
	  ssum += cube[subs][(x+sub+sub)*h+y];
	  ssum += cube[subs][x*h+(y+sub)];
	  ssum += cube[subs][x*h+(y+sub+sub)];
	  ssum += cube[subs][(x+sub)*h+(y+sub)];
	  ssum += cube[subs][(x+sub+sub)*h+(y+sub)];
	  ssum += cube[subs][(x+sub)*h+(y+sub+sub)];
	  ssum += cube[subs][(x+sub+sub)*h+(y+sub+sub)];
	  // slow path for prime i
	} else {
	  // get lower dimension square at x+1, y+1
	  ssum = cube[i-1][(x+1)*h+(y+1)];
	  // fill in space above lower square
	  for(int x1=x;x1<x+i+1;x1++) ssum += cube[0][x1*h+y];
	  // fill in left side
	  for(int y1=y+1;y1<y+i+1;y1++) ssum += cube[0][x*h+y1];
	}
	cube[i][x*h+y] = ssum;
	if(ssum > max) {
	  //printf("%d,%d: %d; %d\n", x+1, y+1, ssum, i+1);
	  max = ssum;
	  max_size = i+1;
	  max_x = x+1;
	  max_y = y+1;
	}
      }
    }
  }
  printf("%d,%d,%d\n", max_x, max_y, max_size);

  return 0;
}
