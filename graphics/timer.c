/* timer.c */


#include <sys/time.h>
#include <signal.h>

#include "timer.h"

void (*func)(void);

void sigalrm_hdl(int signo)
{
    func();
}

int start_timer(int msecs, void (*f)(void))
{
    int pid;
    struct itimerval itval;

//    pid = fork();
    
//    if (pid > 0)
//	return pid;

    itval.it_interval.tv_sec = 0;
    itval.it_value.tv_sec = 0;
    itval.it_interval.tv_usec = msecs * 1000;
    itval.it_value.tv_usec = msecs * 1000;

    func = f;

    if (signal(SIGALRM, sigalrm_hdl) == SIG_ERR){
	printf("start_timer can't catch SIGALRM!\n");
	exit(1);
    }

    if (setitimer(ITIMER_REAL, &itval, NULL) != 0){
	printf("timer not set!\n");
	exit(2);
    }
    return 0;

    for(;;){
	pause();
    }
}

int stop_timer(int pid)
{
    struct itimerval itval;

//    kill(pid, 9);

    itval.it_interval.tv_sec = 0;
    itval.it_value.tv_sec = 0;
    itval.it_interval.tv_usec = 0;
    itval.it_value.tv_usec = 0;

    if (setitimer(ITIMER_REAL, &itval, NULL) != 0){
	printf("timer not set!\n");
	exit(2);
    }

    return 0;
}
