#include <stdio.h>
#include <time.h>
#include "TempAlert.h"

#define AVE_TMP 22.0

struct timespec t;

void Input(XFRP_FLOAT *tmp) {
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
  if (t.tv_sec % 2) {
    *tmp = AVE_TMP - 0.5 + (float)t.tv_nsec / 1e9;
  } else{
    *tmp = AVE_TMP + 0.5 - (float)t.tv_nsec / 1e9;
  }
}

void Output(XFRP_BOOL *alert) {
  if (*alert) {
    fprintf(stderr, "ALERT!!!\n");
  }
}

int main(int argc, char *argv[]) {
  ActivateTempAlert();
  return 0;
}
