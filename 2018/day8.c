#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void go(int *data) {
  size_t *stack = calloc(4096*8, sizeof(size_t));
  int sp=0;
  int sum=0;
  int i=0;
  int n=0;
  int cs;
  int ms;
 parse_node:
  cs = data[i++];
  ms = data[i++];
  for(n=0;n<cs;n++) {
    stack[sp++] = cs;
    stack[sp++] = ms;
    stack[sp++] = n;
    goto parse_node;
  resume:
    n = stack[--sp];
    ms = stack[--sp];
    cs = stack[--sp];
  }
  for(n=0;n<ms;n++) sum += data[i+n];
  i+= ms;
  if(sp>0) goto resume;

  printf("%d\n", sum);

  int *children;
  i = 0;
 make_node:
  cs = data[i++];
  ms = data[i++];
  children = calloc(cs, sizeof(int));
  for(n=0;n<cs;n++) {
    stack[sp++] = cs;
    stack[sp++] = ms;
    stack[sp++] = n;
    stack[sp++] = (size_t)children;
    goto make_node;
  resume_make:
    children = (int*)stack[--sp];
    n = stack[--sp];
    ms = stack[--sp];
    cs = stack[--sp];
    children[n] = sum;
  }
  sum = 0;
  if(cs == 0)
    for(n=0;n<ms;n++) sum += data[i+n];
  else
    for(n=0;n<ms;n++)
      if(data[i+n]-1 < cs)
	sum += children[data[i+n]-1];
  i += ms;
  free(children);
  if(sp>0) goto resume_make;

  printf("%d\n", sum); 
}

int main() {
  int *data = calloc(4096*8, sizeof(int));
  int len = 0;
  while(scanf("%d", &data[len++]) > 0) getchar();
  go(data);
  return 0;
}
