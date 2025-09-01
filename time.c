#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    time_t t;
    char *s;
    t = time(NULL);
    s = ctime(&t);
    printf("%ld\n", t);
    printf("%s\n", s);
    return 0;
}