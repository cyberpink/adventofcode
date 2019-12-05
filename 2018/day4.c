#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "util.c"

enum event_type {START = 0, WAKE = 1, SLEEP = 2}; 
typedef struct event {
  int month;
  int day;
  int hour;
  int minute;
  int event;
  int guard;
} event;

int compare_events(const void *p, const void *q) {
  event x = **(event **)p;
  event y = **(event **)q;
  if (x.month < y.month) return -1;
  if (x.month > y.month) return 1;
  if (x.day < y.day) return -1;
  if (x.day > y.day) return 1;
  if (x.hour < y.hour) return -1;
  if (x.hour > y.hour) return 1;
  if (x.minute < y.minute) return -1;
  if (x.minute > y.minute) return 1;
  return 0;
}

void print_event(event e) {
  printf("[%02d-%02d %02d:%02d] %d, %d\n", e.month, e.day, e.hour, e.minute, e.event, e.guard);
}

void run(event **list, int len) {
  int i, n, c, max, mi;
  int guard_id = 0, sleep_start = -1, sleep_end;
  int *sleepy_hours = calloc(4096, sizeof(int));
  int **midnight_hours = calloc(4096, sizeof(int**));;
  int *m;
  event e;

  qsort(list, len, sizeof(event*), &compare_events);

  for(i=0;i<len;i++) {
    e = *list[i];
    switch(e.event) {
    case START:
      guard_id = e.guard;
      sleep_start = -1;
      break;
    case SLEEP:
      sleep_start = e.minute;
      break;
    case WAKE:
      sleep_end = e.minute; 
      m = midnight_hours[guard_id];
      if(m == NULL) {
	m = calloc(60, sizeof(int));
	midnight_hours[guard_id] = m;
      }
      for(n=sleep_start;n<sleep_end;n++) m[n]++;
      sleepy_hours[guard_id] += sleep_end - sleep_start;
      break;
    }
  }
  max = 0;
  mi = 0;
  for(i=0;i<4096;i++) {
    c = sleepy_hours[i];
    if(c > max) { max = c; mi = i; }
  }
  //printf("%d,%d\n", mi, max);

  m = midnight_hours[mi];
  guard_id = mi;
  max = 0;
  mi = 0;
  for(i=0;i<60;i++) {
    c = m[i];
    if(c > max) { max = c; mi = i; }
  }
  printf("minutes[%d]=%d*%d=%d\n", mi,max,guard_id, mi * guard_id);

  mi = 0;
  max = 0;
  guard_id = 0;
  for(i=0;i<4096;i++) {
    m = midnight_hours[i];
    if (m == NULL) continue;
    for(n=0;n<60;n++) {
      c = m[n];
      if(c > max) { max = c; mi = n; guard_id = i; }
    }
  }
  printf("minutes[%d]=%d*%d=%d\n", mi,max,guard_id, mi * guard_id);
}

int main() {
  int year, month, day, hour, minute, event_id, guard_id;
  char tail[50];

  int i = 0;
  event **events = calloc(4096, sizeof(event));
  event *p;
  int c = 0;

  while(scanf("[%d-%d-%d %d:%d] ", &year, &month, &day, &hour, &minute) > 0) {
    c = 0;
    while(1) {
      tail[c] = getchar();
      if(tail[c] == '\n') { tail[c] = '\0'; break; }
      c++;
    }
    switch (tail[0]) {
    case 'f': event_id = SLEEP; break;
    case 'w': event_id = WAKE; break;
    case 'G':
      event_id = START;
      sscanf(tail, "Guard #%d begins shift", &guard_id);
      break;
    default:
      printf("bad tail: %s\n", tail);
      exit(0);
    }
    p = malloc(sizeof(event));
    *p = (event){ month, day, hour, minute, event_id, guard_id };
    events[i++] = p;
  }

  run(events, i);
}
