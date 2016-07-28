// man 2 setitimer

#include <math.h>
#include <X11/Xlib.h>
#include <X11/xpm.h>

#include "timer.h"

#define BSIZE 32
#define BVDISP 28
#define FIELDWIDTH 10
#define FIELDHEIGHT 20
#define MAINW FIELDWIDTH * BSIZE * 2
#define MAINH FIELDHEIGHT * BVDISP + BSIZE * 4
#define BORDERW 2

#define VEL 4.0

#include "balls/red-ball.xpm"
#include "balls/blue-ball.xpm"
#include "balls/green-ball.xpm"
#include "balls/yellow-ball.xpm"
#include "balls/purple-ball.xpm"
#define BALLS 5

#include "balls/icon-face.xpm"


#define IS_ODD(n) ((n) % 2)

Display *disp;
Window win, main_win;
int board[FIELDHEIGHT][FIELDWIDTH];
Pixmap ball_pix[BALLS];
Pixmap ball_mask;
GC gc;

float animx, animy, xvel, yvel;
int animc, animnc, animb;

void animate();
void potentiate(int* wouldbex, int* wouldbey);
int touches(int wouldbex, int wouldbey);
double pythagorate(double x, double hyp)
{
    return sqrt(hyp * hyp - x * x);
}


int main()
{
    int screen_num;
    Pixmap icon;
    char** ball_data[BALLS] = {
	red_ball_xpm,
	blue_ball_xpm,
	green_ball_xpm,
	yellow_ball_xpm,
	purple_ball_xpm};
    int i;

    XSizeHints *size_hints;
    XWMHints *wm_hints;
    XTextProperty windowName, iconName;
    char *window_name = "krude";

    srand(time(NULL));

    for (i = 0; i < 8  * FIELDWIDTH; i++){
	    board[i / FIELDWIDTH][i % FIELDWIDTH] = rand() % BALLS;
    }
    for (i; i < FIELDHEIGHT * FIELDWIDTH; i++){
	    board[i / FIELDWIDTH][i % FIELDWIDTH] = -1;
    }

    animnc = rand() % BALLS;

    disp = XOpenDisplay(NULL);
    if (disp == NULL){
	printf("Where is my X?  I need my X!\n");
	exit(1);
    }

    screen_num = DefaultScreen(disp);

    main_win = XCreateSimpleWindow(disp, RootWindow(disp, screen_num), 0, 0, MAINW, MAINH, 0, 0, BlackPixel(disp, screen_num));
    win = XCreateSimpleWindow(disp, main_win, FIELDWIDTH * BSIZE / 2, 0, FIELDWIDTH * BSIZE, (FIELDHEIGHT - 1) * BVDISP + BSIZE, BORDERW, WhitePixel(disp, screen_num), BlackPixel(disp, screen_num));

    for (i = 0; i < BALLS; i++){
	if(XpmCreatePixmapFromData(disp, win, ball_data[i], &ball_pix[i], &ball_mask, NULL) != XpmSuccess){
	    printf("oops.\n");
	    exit(2);
	}
    }

    XpmCreatePixmapFromData(disp, win, icon_face_xpm, &icon, NULL, NULL);

    if (!((size_hints = XAllocSizeHints()) && (wm_hints = XAllocWMHints()))){
	printf("doh!\n");
	exit(3);
    }

    size_hints->flags = PSize | PMinSize | PMaxSize;
    size_hints->min_width = size_hints->max_width = MAINW;
    size_hints->min_height = size_hints->max_height = MAINH;

    if (XStringListToTextProperty(&window_name, 1, &windowName) == 0){
	printf("doh!\n");
	exit(4);
    }

    wm_hints->initial_state = NormalState;
    wm_hints->input = True;
    wm_hints->icon_pixmap = icon;
    wm_hints->flags = StateHint | IconPixmapHint | InputHint;


    XSetWMProperties(disp, main_win, &windowName, NULL, NULL, 0, size_hints, wm_hints, NULL);

    gc = XCreateGC(disp, win, 0, NULL);

    XSetClipMask(disp, gc, ball_mask);

    XSelectInput(disp, main_win, ExposureMask | SubstructureNotifyMask | KeyPressMask | ButtonPressMask);

    XMapWindow(disp,win);
    XMapWindow(disp, main_win);

    for(;;){
	XEvent report;
	int j;

	XNextEvent(disp, &report);
	switch (report.type){
	case Expose:
	    for (j = 0; j < FIELDHEIGHT; j++){
		for (i = 0; i < FIELDWIDTH; i++){
		    if (board[j][i] >= 0){
			XSetClipOrigin(disp, gc, i * BSIZE, j * BVDISP);
			XCopyArea(disp, ball_pix[board[j][i]], win, gc, 0, 0, BSIZE, BSIZE, i * BSIZE, j * BVDISP);
		    }
		}
		j++;
		for (i = 0; i < FIELDWIDTH - 1; i++){
		    if (board[j][i] >= 0){
			XSetClipOrigin(disp, gc, BSIZE / 2 + i * BSIZE, j * BVDISP);
			XCopyArea(disp, ball_pix[board[j][i]], win, gc, 0, 0, BSIZE, BSIZE, BSIZE / 2 + i * BSIZE, j * BVDISP);
		    }
		}
	    }

	    if (animb){
		XSetClipOrigin(disp, gc, animx, animy);
		XCopyArea(disp, ball_pix[animc], win, gc, 0, 0, BSIZE, BSIZE, animx, animy);
	    }

	    XSetClipOrigin(disp, gc, MAINW - (FIELDWIDTH * BSIZE / 2) + BORDERW, MAINH - (BSIZE * 5) + 4 + (BORDERW * 2));
	    XCopyArea(disp, ball_pix[animnc], main_win, gc, 0, 0, BSIZE, BSIZE, MAINW - (FIELDWIDTH * BSIZE / 2) + BORDERW, MAINH - (BSIZE * 5) + 4 + (BORDERW * 2));
	    
	    break;
	case KeyPress:
	case ButtonPress:
	    if (animb)
		continue;

	    animx = (FIELDWIDTH - 1) * BSIZE / 2 + 1;
	    animy = (FIELDHEIGHT - 1) * BVDISP;
	    animc = animnc;
	    animnc = rand() % BALLS;
	    animb = 1;
	    xvel = (((rand() % 78) + 1 )/ 10.0) - 4;
	    yvel = pythagorate(xvel, VEL);

	    start_timer(10, animate);

	    XSetClipOrigin(disp, gc, MAINW - (FIELDWIDTH * BSIZE / 2) + BORDERW, MAINH - (BSIZE * 5) + 4 + (BORDERW * 2));
	    XCopyArea(disp, ball_pix[animnc], main_win, gc, 0, 0, BSIZE, BSIZE, MAINW - (FIELDWIDTH * BSIZE / 2) + BORDERW, MAINH - (BSIZE * 5) + 4 + (BORDERW * 2));

	    XSync(disp, False);

//	    fire();
	    break;
	}
    }
}

void animate()
{

    /* FIXME: create 2 new pixmaps, each big enough for two balls in relative positions for
       current and previous frame. fill one with black, draw new ball.  fill other with black,
       draw ball_mask in old and new positions.  copy 1st with mask of second.

       this should get rid of flicker without leaving trail.
    */
    
    int wouldbex, wouldbey;
    Pixmap buf, bufmask;
    
//    printf("%f\n", animy);
    
//    XClearArea(disp, win, 0, animy + BSIZE, FIELDWIDTH * BSIZE, FIELDHEIGHT + BSIZE - animy, False);
    XClearArea(disp, win, animx, animy, BSIZE, BSIZE, False);

    animx += xvel;
    animy -= yvel;

    if (animx + BSIZE >= BSIZE * FIELDWIDTH){
	xvel = -1 * fabs(xvel);
    }

    if (animx <= 0){
	xvel = fabs(xvel);
    }

    potentiate(&wouldbex, &wouldbey);

    if (touches(wouldbex, wouldbey)){
	stop_timer(0);
	animb = 0;

	board[wouldbey][wouldbex] = animc;

//	XClearArea(disp, win, animx, animy, BSIZE, BSIZE, False);

	
//	XSetClipMask(disp, gc, None);
//	XFillRectangle(disp, win, gc, 0, 0, 16, 16);
//	XSetClipMask(disp, gc, ball_mask);

	
	if (wouldbey % 2){ /* odd */
	    XSetClipOrigin(disp, gc, BSIZE / 2 + wouldbex * BSIZE, wouldbey * BVDISP);
	    XCopyArea(disp, ball_pix[animc], win, gc, 0, 0, BSIZE, BSIZE, BSIZE / 2 + wouldbex * BSIZE, wouldbey * BVDISP);
	}
	else{ /* even */
	    XSetClipOrigin(disp, gc, wouldbex * BSIZE, wouldbey * BVDISP);
	    XCopyArea(disp, ball_pix[animc], win, gc, 0, 0, BSIZE, BSIZE, wouldbex * BSIZE, wouldbey * BVDISP);
	}

	XSync(disp, False);
	return;
    }
    
//    XSetClipMask(disp, gc, None);
    XSetClipOrigin(disp, gc, animx, animy);
    XCopyArea(disp, ball_pix[animc], win, gc, 0, 0, BSIZE, BSIZE, animx, animy);
//    XSetClipMask(disp, gc, ball_mask);
    XSync(disp, False);
}

/* use animx and animy to determine the position in board[][]
   the moving ball is closest to.
*/
void potentiate(int* wouldbex, int* wouldbey)
{
    *wouldbex = animx / BSIZE;
   if ((int)animx % BSIZE > BSIZE / 2) (*wouldbex)++;
   if (IS_ODD(*wouldbey) && *wouldbex == FIELDWIDTH - 1)
	(*wouldbex)--;
    *wouldbey = animy / BVDISP;
    if ((int)animy % BVDISP > BVDISP / 2) (*wouldbey)++;
}


/* FIXME: write this right */
/* decide if moving ball makes contact with any still balls */
int touches(int x, int y)
{
    if (y == 0)
	return 1;

    if (board[y - 1][x] >= 0)
	return 1;

    if (IS_ODD(y) && board[y - 1][x + 1] >= 0)
	return 1;

    if (!IS_ODD(y) && board[y - 1][x - 1] >= 0)
	return 1;

    if (xvel > 0 && x < FIELDWIDTH - 1 && board[y][x + 1] >= 0)
	return ;

    if (xvel < 0 && x > 0 && board[y][x - 1] >= 0)
	return ;

    return 0;
}
