/* timer.h */

/* BE SURE TO CALL stop_timer() BEFORE EXITING PROGRAM!
 */

int start_timer(int msecs, void (*func)(void));
int stop_timer(int pid);
