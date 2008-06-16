#include <math.h>

#define ln2 0.6931472

double _entropy(double x) {
    if (x == 0.0 || x == 1.0)
        return 0.0;
    else
        return -((1.0 - x) * log(1.0 - x) + x * log(x)) / ln2;
}

void _infotest(int *A, int *B, double *num, double *nx, double *info) {
    double xleftA = 0.0,
        xleftB = 0.0,
        numi, a, tmp;
    int i;

    *info = 1.0;

    for (i = 1; i < *num; i++) {
        if (A[i - 1] == 1) xleftA++;
        if (B[i - 1] == 1) xleftB++;
        numi = *num - (double)i;
        a = (double)i / *num;
        tmp = a * _entropy(xleftA / (double)i) + (1.0 - a) * _entropy((*nx - xleftA) / numi);
        if (tmp < *info) *info = tmp;
        tmp = a * _entropy(xleftB / (double)i) + (1.0 - a) * _entropy((*nx - xleftB) / numi);
        if (tmp < *info) *info = tmp;
    }
}
