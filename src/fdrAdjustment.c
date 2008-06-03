#include <math.h>

void fdrAdjust(double *q, int *n, int *stepUp) {
    int i;
    double p = 1;

    if (*stepUp) {
        for (i = *n - 1; i >= 0; i--) {
            if (q[i] < p) p = q[i];
            q[i] = p;
        }
    } else {
        for (i = 0; i < *n; i++) {
            if (q[i] > p) p = q[i];
            q[i] = p;
        }
    }
}
