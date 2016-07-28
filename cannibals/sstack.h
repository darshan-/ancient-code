/* sstack.h - stack of states we've acheived */

int add_state(int c, int m, int b);
int rm_state(void);
int been_there(int c, int m, int b);
void dump_stack(void);
