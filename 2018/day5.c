#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util.c"

int match(char a, char b) { return (a ^ b) == 32; }

int main() {
  char *in = calloc(65536, sizeof(char));
  char *out = calloc(65536, sizeof(char));
  char *swap;
  char c;
  int in_end = 0, out_end = 0, i = 0, n = 0;

  while(1) {
    c = getchar(); 
    if(c == EOF) break;
    in[in_end++] = c;
  }

  for(i=0;i<in_end;i++) {
    if(match(in[i], out[out_end])) out_end--;
    else out[++out_end] = in[i];
  }
  printf("%d\n", out_end-1);

  int min = out_end;
  in_end = out_end;
  swap = in;
  in = out;
  out = swap;
    
  for(n=65;n<123;n++) {
    out_end = 0;
    out[0] = '\0';
    for(i=0;i<in_end;i++) {
      if((char)n == in[i] || match(in[i], (char)n)) continue;
      else if(match(in[i], out[out_end])) out_end--;
      else out[++out_end] = in[i];
    }
    if(out_end < min) min = out_end;
  }
  printf("%d\n", min-1);

  return 0;
}
