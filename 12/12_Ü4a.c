#include <stdio.h>

int main() {
    int x1, x2;
    scanf("%i", &x1);
    x2 = 1;
    while (x1 > 0) {
        x2 = x2 * 2;
        x1 = x1 - 1;
    }
    printf("%d", x2);
    return 0;
}
