#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

const int d = 2048;
void go(char *g, int ymax) {
    char c;
    int fx, fy, mx, my;
    int left_bound, right_bound;
    int right_boundp, left_boundp;

    int *stack = calloc(8192, sizeof(int));
    int sp = 0;

    stack[sp++] = 500;
    stack[sp++] = 0;
    run:
        if(sp == 0) goto done;
        fy = stack[--sp];
        fx = stack[--sp];
    fall:
        fy++;
        if(fy>ymax) goto run; 
        c = g[fy*d+fx];
        if(c == '.') { g[fy*d+fx] = '|'; goto fall; }
        else if(c == '|') { goto run; }
        else if(c == '#' || c == '~') { goto fill_loop; }
    fill_loop:
        fy--;
        c = g[fy*d+fx];
        if(c == '.') { g[fy*d+fx] = '|'; }
        mx = fx;
        my = fy;
        left_boundp = 0;
        right_boundp = 0;
        scan_left:
            fx--;
            c = g[fy*d+fx];
            if(c == '.') { g[fy*d+fx] = '|'; }
            if(g[(fy+1)*d+fx] == '|') goto run;
            else if(g[(fy+1)*d+fx] == '.') {
                left_bound = fx;
                goto start_scan_right;
            }
            else if(c == '#') {
                left_bound = fx;
                left_boundp = 1;
                goto start_scan_right;
            }
            goto scan_left;
        start_scan_right:
            fx = mx;
        scan_right:
            fx++;
            c = g[fy*d+fx];
            if(c == '.') { g[fy*d+fx] = '|'; }
            if(g[(fy+1)*d+fx] == '|') goto run;
            else if(g[(fy+1)*d+fx] == '.') {
                right_bound = fx;
                goto scan_done;
            }
            else if(c == '#') {
                right_bound = fx;
                right_boundp = 1;
                goto scan_done;
            }
            goto scan_right;
        scan_done:
            if(left_boundp && right_boundp) {
                for(int x=left_bound+1;x<right_bound;x++) g[fy*d+x] = '~';
                fx = mx;
                fy = my;
                goto fill_loop;
            } else {
                if(!right_boundp) {
                    stack[sp++] = right_bound;
                    stack[sp++] = fy;
                }
                if(!left_boundp) {
                    stack[sp++] = left_bound;
                    stack[sp++] = fy;
                }
                goto run;
            }
    done:
    return;
}

int main(void) {
    char v1n, v2n;
    int v1, v2s, v2e;
    int spring = 0*d+500;
    char *g = calloc(d*d, sizeof(char));
    for(int y=0;y<d;y++) for(int x=0;x<d;x++) g[y*d+x] = '.';
    g[spring] = '+';
    int ymin = INT32_MAX;
    int ymax = 0;
    while(scanf("%c=%d, %c=%d..%d\n", &v1n, &v1, &v2n, &v2s, &v2e) > 0) {
        if(v1n == 'x') {
            ymin = v2s < ymin ? v2s : ymin;
            ymax = v2e > ymax ? v2e : ymax;
            for(int i=v2s;i<v2e+1;i++) g[i*d+v1] = '#';
        } else {
            ymin = v1 < ymin ? v1 : ymin;
            ymax = v1 > ymax ? v1 : ymax;
            for(int i=v2s;i<v2e+1;i++) g[v1*d+i] = '#';
        }
    }
    go(g, ymax);

    int count = 0, retained = 0;
    for(int y=ymin;y<ymax+1;y++)
        for(int x=0;x<d;x++) {
            if(g[y*d+x]=='|') count++;
            if(g[y*d+x]=='~') { count++; retained++; }
        }
    printf("COUNT: %d, RETAINED: %d\n", count, retained);

    /* for(int y=ymin;y<d;y++) { */
    /*   for(int x=0;x<d;x++) printf("%c", g[y*d+x]); */
    /*   printf("\n"); */
    /* } */

    return 0;
}
