/* sstack.c - stack of states we've acheived */

#include <stdlib.h>

#include "sstack.h"

/* the state of our side of the river.
   there is no need to define a state when the boat is in transit,
   so the state of the other side of the river can be inferred
   from the state of our side.
*/
struct state{
    int c;    /* cannibals */
    int m;    /* missionaries */
    int b;    /* boats ;-) */
};

struct node{
    struct state state;
    struct node* prev;
};

struct node* tail = NULL;

int add_state(int c, int m, int b)
{
    struct node* np = malloc(sizeof(struct node));
    if (np == NULL){
	return 1;
    }

    np->state.c = c;
    np->state.m = m;
    np->state.b = b;
    np->prev = tail;
    tail = np;

    return 0;
}

int rm_state(void)
{
    struct node* p;

    if (tail == NULL){
	return 1;
    }

    p = tail;
    tail = tail->prev;

    free(p);

    return 0;
}

int been_there(int c, int m, int b)
{
    struct node* p;

    for (p = tail; p != NULL; p = p->prev){
	if(p->state.c == c &&
	   p->state.m == m &&
	   p->state.b == b)
	{
	    return 1;
	}
    }
    
    return 0;
}

void dump_stack(void)
{
    struct node* p;

    for (p = tail; p != NULL; p = p->prev){
	printf("\t%d, %d, %d\n", p->state.c, p->state.m, p->state.b);
    }
}
