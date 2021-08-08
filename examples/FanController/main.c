/* From psg_titech/emfrp_samples/SimFanController:
 * https://github.com/psg-titech/emfrp_samples/blob/824f4de95ceb6b535deb8b4dd8f875998891f2e5/SimFanController/src/main.c */
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "FanController.h"

#define SBUFLEN 100

double t = 30.0, h = 60.0;
double dt = 0.5, dh = 1.0;

void Input(XFRP_FLOAT *tmp, XFRP_FLOAT *hmd) {
  if (t > 35.0 || t < 20.0) dt = -dt;
  if (h > 80.0 || h < 50.0) dh = -dh;
  t += dt;
  h += dh;
  *tmp = t;
  *hmd = h;
}

void Output(XFRP_BOOL *fan, XFRP_FLOAT *di) {
  static char sbuf[SBUFLEN];
  snprintf(sbuf, SBUFLEN,
          "tmp=%2.2f, hmd=%2.2f, di=%2.2f, fan: %-3s",
          t, h, *di, *fan ? "ON" : "OFF");
  printf("%s", sbuf);
  fflush(stdout);
  int n = strlen(sbuf);
  for (int i = 0; i < n; i++)
      putchar(010);
  usleep(200000);
}

int main(int argc, char *argv[]) {
  ActivateFanController();
  return 0;
}
