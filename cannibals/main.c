#include <stdlib.h>
#include <stdio.h>

#include "sstack.h"

#define B 1

void move(int c, int m, int b);

int C = 3;
int M = 3;

int wins = 0;

int main(int argc, char* argv[])
{
	if (argc == 2 || argc > 3)
	{
		/* Make sure we get either 0 or 2 arguments.  I'm not doing
		   anything more sophisticated as this is just a toy.
		*/
		fprintf(stderr, "Bad arguments.\n");
		exit(1);
	}
	
    if (argc == 3)
	{
		C = atoi(argv[1]);
		M = atoi(argv[2]);
    }

    move(C, M, B);

    printf("\n%d ways to win.\n", wins);
    
    return 0;
}

void move(int c, int m, int b)
{
    if ((c == 0) && (m == 0)){
	wins++;

	printf("I won as follows:\n");
	
	add_state(c, m, b);
	dump_stack();
	rm_state();

	return;
    }
    
    if (been_there(c, m, b)){
	return;
    }
    
    if ((m != 0) && (c > m)){ /* dinner on start side */
	return;
    }

    if ((m != M) &&(C - c > M - m)){ /* dinner of goal side */
	return;
    }

    add_state(c, m, b);
    
    if (b == 1){
	if (c >= 2){
	    move(c-2, m, 0);
	}
	
	if (m >= 2){
	    move(c, m-2, 0);
	}

	if ((c > 0) && (m > 0)){
	    move(c-1, m-1, 0);
	}

	if (c > 0){
	    move(c-1, m, 0);
	}

	if (m > 0){
	    move(c, m-1, 0);
	}
    }
    else{  /* (b == 0)*/
	if (m < M){
	    move(c, m+1, 1);
	}

	if (c < C){
	    move(c+1, m, 1);
	}

	if ((c < C) && (m < M)){
	    move(c+1, m+1, 1);
	}

	if ((m <= 1) && (m+2 <= M)){
	    move(c, m+2, 1);
	}

	if ((c <= 1) && (c+2 <= C)){
	    move(c+2, m, 1);
	}
    }

    rm_state();
}
