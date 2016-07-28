#include <stdio.h>

int main()
{
    int i;
    int sum = 0;

    for (i = 1; i < 364; i++){
	sum += i;
    }

    printf("%d\n", sum);

    return 0;
}
