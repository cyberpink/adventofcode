#include <stdio.h>

int main(void) {
  int buf;
  long int part1 = 0;
  long int part2 = 0;
  while(scanf("%d\n", &buf) != EOF) {
    int last = buf;
    part1 += (buf / 3) - 2;
    while(1) {
      last = (last / 3) - 2;
      if(last > 0) {
	part2 += last;
      } else {
	break;
      }
    }
  }
  printf("part 1: %ld\n", part1);
  printf("part 2: %ld\n", part2);
  return 0;
}
