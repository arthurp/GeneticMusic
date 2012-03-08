#include <stdio.h>
#include <math.h>

#undef min
#undef max

double max(double x, double y) {
  return x > y ? x : y;
}
double min(double x, double y) {
  return x < y ? x : y;
}
double logistic(double x) {
  // 1/(1+e^(-x))
  return 1 / (1 + exp(-x));
}

void writeSample(double d) {
  fwrite(&d, sizeof(d), 1, stdout);
}

int main(int argc, char* argv[]) {
  int n = atoi(argv[1]);
  generate(n);
  return 0;
}
