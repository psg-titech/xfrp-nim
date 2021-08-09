#include <iostream>
#include <ctime>
#include "TempAlert.h"

#define AVE_TMP 25.0

struct timespec t;

void Input(XFRP_FLOAT *tmp) {
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
  if (t.tv_sec % 2) {
    *tmp = AVE_TMP - 0.5 + (float)t.tv_nsec / 1e9;
  } else {
    *tmp = AVE_TMP + 0.5 - (float)t.tv_nsec / 1e9;
  }
}

void Output(XFRP_BOOL *alert) {
  if (*alert) {
    std::cerr << "ALERT!!!" << std::endl;
  }
}

int main(int argc, char *argv[]) {
  ActivateTempAlert();
  return 0;
}
