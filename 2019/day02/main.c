#include <stdio.h>
#include <assert.h>
#include <malloc.h>
#include <string.h>

int run(int* ops, size_t size, int n, int v) {
  
  ops[1] = n;
  ops[2] = v;
  
  int opi = 0;
  while(1) {
    switch (ops[opi])  {
    case 1:
      ops[ops[opi+3]] = ops[ops[opi+1]] + ops[ops[opi+2]];
      break;
    case 2:
      ops[ops[opi+3]] = ops[ops[opi+1]] * ops[ops[opi+2]];
      break;
    case 99:
      return ops[0];
    default: assert(0);
    }
    opi += 4;
  }
}

int part1(int *_ops, size_t size) {
  int* ops = malloc(size);
  memcpy(ops, _ops, size);
  return run(ops, size, 12, 2);  
}

int part2(int *_ops, size_t size) {
  int* ops = malloc(size);
  for(int n=0;n<100;n++) {
    for(int v=0;v<100;v++) {
      memcpy(ops, _ops, size);
      if(run(ops, size, n, v) == 19690720)
	return 100 * n + v;
    }
  }
    
}
  
int main() {  
  int ops[200];
  int opi = 0;
  
  while(1) {
    scanf("%d,%d,%d,%d,", &ops[opi+0], &ops[opi+1], &ops[opi+2], &ops[opi+3]);
    //printf("%d,%d,%d,%d,\n", ops[opi+0], ops[opi+1], ops[opi+2], ops[opi+3]);
    if (ops[opi+0] == 99)
      break;
    opi+=4;
  }  

  printf("part 1: %d\n", part1(ops, sizeof(ops)));
  printf("part 2: %d\n", part2(ops, sizeof(ops)));
  
  return 0;
}
