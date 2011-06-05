#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

double gettime(void)
{
    struct timeval tv;

    gettimeofday(&tv, NULL);

    return tv.tv_sec + (tv.tv_usec / 1e6);
}

void loop(int count)
{
    int i;

    for (i = 0; i < count; i++)
	printf("hi mom %g\n", (double) i * M_PI);
}

int main(int argc, char **argv)
{
    double start, elapsed;
    int i, count;

    count = argc == 2 ? atoi(argv[1]) : 1600000;

    start = gettime();
    
    loop(count);

    elapsed = gettime() - start;

    fprintf(stderr, "%d iterations in %g secs (%g thousand/sec)\n",
	    count, elapsed, count / elapsed / 1e3);
}
