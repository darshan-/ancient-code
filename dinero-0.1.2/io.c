/* io.c */

/* This file is part of Dinero */
/* Copyright (C) 2001 by Josiah Israel Barber */

 /*****************************************************************************
 *                                                                            *
 *   This program is free software; you can redistribute it and/or modify     *
 *   it under the terms of the GNU General Public License as published by     *
 *   the Free Software Foundation; either version 2 of the License, or        *
 *   (at your option) any later version.                                      *
 *                                                                            *
 *   This program is distributed in the hope that it will be useful,          *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *   GNU General Public License for more details.                             *
 *                                                                            *
 *   You should have received a copy of the GNU General Public License        *
 *   along with this program; if not, write to the Free Software              *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA*
 *                                                                            *
 *                                                                            *
 *   See file 'COPYING' for complete details.                                 *
 *                                                                            *
 *****************************************************************************/

#include <curses.h>
#include <signal.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>


#include "account.h"
#include "session.h"
#include "io.h"


void my_refresh(){
    char balance[20];

    strcpy(balance, get_balance(current_account->Account));
    
    attrset(A_REVERSE);

    clear_line(LINES - 2);
    move(LINES - 2, 0);
    printw("-%c%c-L%d", current_account->modified ? '*' : '-', current_account->modified ? '*' : '-', current_account->cur_line);
    printw("--B%d", get_bottom_entry_num(current_account->Account));
    printw("--D(%d-%d)", current_account->line_offset, current_account->line_offset + (LINES - 3));
    printw("  %s  ", current_account->account_name);

    /*  if(current_account->line_offset == 0 && get_bottom_entry_num(current_account->Account) < LINES - 2){
		printw("--All");
		}else if(current_account->line_offset == 0){
		printw("--Top");
		}else if(get_bottom_entry_num(current_account->Account) < LINES - 2){
		printw("--Bot");
		}else{
		printw("--%%%d", (100 * current_account->cur_line) / get_bottom_entry_num(current_account->Account));
		}
    */

    attrset(0);

    if(balance[0] != '\0'){
		move(LINES - 2, COLS - strlen(balance) - 1);

		if(balance[0] == '+') echochar('+');
		else echochar('-');

		printw("$%s", balance + 1);
    }

    move(LINES - 1, COLS - 1);

    refresh();
}

void clear_line(int line){
    int i;

    move(line, 0);
    for(i = 0; i < COLS; i++)
		addch(' ');
}

void update_screen(){
    int i;
    entry* Entry;

    erase();

    for(i=0; i < LINES - 2 && i + current_account->line_offset <= get_bottom_entry_num(current_account->Account); i++){
		Entry = get_entry(current_account->Account, i + current_account->line_offset);
		if(! Entry) continue;

		move(i, 0);
		printw(Entry->Date);

		move(i, DATE_WIDTH + 1);
		printw("%.*s", COLS - DATE_WIDTH - 1 - MONEY_WIDTH - 2, Entry->Comment);

		if(Entry->Money[0]){
			move(i, COLS - MONEY_WIDTH - 1);
			addch(Entry->Money[0]);              /* sign */

			move(i, COLS - strlen(Entry->Money));
			addch('$');
			printw(Entry->Money + 1);            /* everything after sign */
		}
    }

    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);
}

void sig_winch_handle(int sig_num){
    int c;

    old_sig_winch(sig_num);

    timeout(0);
    for(c = 1; c != ERR; c = getch()) ;
    timeout(-1);

    ungetch(KEY_RESIZE);
}

void sig_winch(){
    while(COLS < DATE_WIDTH + MONEY_WIDTH + 4 || LINES < 3){
        erase();

        move(0, 0);
        printw("Too small!");

        refresh();
        getch();
    }

    if(current_account->cur_line - current_account->line_offset > LINES - 3){
        current_account->line_offset = current_account->cur_line - (LINES - 3);

		update_screen();
		my_refresh();
    }
}

void set_field_attrib(int y, int x, int attrib){
    char read_in[COLS];
    int field_length;

    if(x == 0) field_length = DATE_WIDTH;
    else if(x == 1) field_length = COLS - DATE_WIDTH - 1 - MONEY_WIDTH - 2;
    else field_length = MONEY_WIDTH;

    if(x == 1) x = DATE_WIDTH + 1;
    else if(x == 2) x = COLS - MONEY_WIDTH;

    if(field_length > COLS) field_length=COLS;
    if(field_length < 0) field_length=0;

    move(y, x);
    innstr(read_in, field_length);

    attrset(attrib);
    printw(read_in);
    attrset(0);
}

int my_getch(){
    int c;

    for(;;){              /* keep going until we get something worth returning */
		timeout(0);
		c = getch();
		if(c == ERR) continue;
		timeout(-1);
	
		if(c >= 32 && c <= 126) return c;                 /* "normal" characters */

		if(c == 3  || c ==  4 || c ==  5   || c ==  7 ||  /* ^C, ^D, ^E, ^G      */
		   c == 8  || c == 21 || c == 15   || c == 19 ||  /* ^H, ^U, ^O, ^S      */
		   c == 11 || c == 32 || c == 24 || c == '\t' || c == '\n')  /* ^K, ^W, ^X, '\t', '\n'  */
			return c;

		if(c == KEY_UP || c == KEY_DOWN || c == KEY_LEFT || c == KEY_RIGHT ||
		   c == KEY_END || c == KEY_HOME || c == KEY_NPAGE || c == KEY_PPAGE)
			return c;

		if(c == 16) return KEY_UP;                        /* ^P                  */
		if(c == 14) return KEY_DOWN;                      /* ^N                  */

		if(c == 6) return KEY_RIGHT;                      /* ^F                  */
		if(c == 2) return KEY_LEFT;                       /* ^B                  */

		if(c == KEY_BACKSPACE) return 8;                  /* same as ^H          */

		if(c == 26 || c == 1){                            /* ^Z, ^A              */
			endwin();
			raise(SIGSTOP);
			refresh();
			continue;
		}

		if(c == KEY_RESIZE || c == 12){                   /* SIGWINCH, ^L        */
			sig_winch();

			update_screen();
			my_refresh();
			return -1;                     /* message buffer needs to be redrawn */
		}
    }
}

int finish_ctrl_x(){
    int c;
    
    message_out("C-x-");

    for(;;){
		c = getch();

		if (c == 7 || c == 3) return c;
	
		if(c == 'z' || c == 26){
			endwin();
			raise(SIGSTOP);
			return c;
		}

		if(c >= 1 && c <= 126){
			return c;
		}

		if(c == KEY_RESIZE || c == 12){                   /* SIGWINCH, ^L        */
			sig_winch();

			update_screen();
			my_refresh();

			message_out("C-x-");

			continue;
		}

    }
}

void message_out(char* message){
    clear_line(LINES - 1);
    move(LINES - 1, 0);
    printw(message);
    refresh();
}

int get_comment(char* comment){
    int c;
    int i;
    int max_chars = min(COLS - DATE_WIDTH - MONEY_WIDTH - 1, COMMENT_WIDTH); 
    char message[] = "Comment: \0";

    char* buffer = (char *) malloc(max_chars + 1);

    message_out(message);

    for(i=0; i < max_chars; i++){
		if(!(buffer[i] = comment[i])) break;
		echochar(buffer[i]);
    }

    for(;;){
		c = my_getch();

		if(c >= 32 && c <= 126 && i < max_chars){
			buffer[i++] = c;
			echochar(c);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			buffer[--i] = '\0';
			message_out(message);
			printw(buffer);
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			buffer[i=0] = '\0';
			message_out(message);
			continue;
		}

		if(c == 3 || c == 7){
			free(buffer);
			return 0;
		}

		if(c == '\n' || c == '\t'){
			buffer[i] = '\0';
			strcpy(comment, buffer);
			free(buffer);

			timeout(0);
			c = getch();
			if(c == ERR) c = KEY_RIGHT;
			ungetch(c);
			timeout(-1);
	       
			return 1;
		}

		if(c == KEY_RIGHT || c == KEY_LEFT || c == KEY_UP || c == KEY_DOWN){
			ungetch(c);
			ungetch('\n');
			continue;
		}

		if(c == -1){                     /* message buffer needs to be redrawn */
			max_chars = min(COLS - DATE_WIDTH - MONEY_WIDTH - 1, COMMENT_WIDTH); 

			buffer = (char *) realloc(buffer, max_chars + 1);
			if(i > max_chars) i = max_chars;

			message_out(message);
			buffer[i] = '\0';
			printw(buffer);
			continue;
		}
    }
}

int get_date(char* date){
    int c;
    int i;
    int max_chars = DATE_WIDTH;
    char message[] = "Date: \0";

    char* buffer = (char *) malloc(max_chars + 1);

    time_t real_time;
    struct tm* cur_time;

    timeout(0);
    c = getch();
    timeout(-1);

    if(c == 't'){
		real_time = time(NULL);
		cur_time = localtime(&real_time);

		buffer[0] = ((cur_time->tm_mon + 1) / 10) + '0';
		buffer[1] = ((cur_time->tm_mon + 1) % 10) + '0';
		buffer[2] = '/';
		buffer[3] = (cur_time->tm_mday / 10) + '0';
		buffer[4] = (cur_time->tm_mday % 10) + '0';
		buffer[5] = '/';
		buffer[6] = ((cur_time->tm_year % 100) / 10) + '0';
		buffer[7] = ((cur_time->tm_year % 100) % 10) + '0';
		buffer[8] = '\0';

		strcpy(date, buffer);
		free(buffer);

		timeout(0);
		c = getch();
		if(c == ERR) c = KEY_RIGHT;
		ungetch(c);
		timeout(-1);

		return 1;
    }

    if(c != ERR) ungetch(c);

    message_out(message);

    for(i=0; i < max_chars; i++){
		if(!(buffer[i] = date[i])) break;
		echochar(buffer[i]);
    }

    for(;;){
		c = my_getch();

		if(c >= '0' && c <= '9' && i < max_chars){
			if((i == 0 && c > '1') || (i == 3 && c > '3')){
				buffer[i++] = '0';
				echochar('0');

				buffer[i++] = c;
				echochar(c);

				buffer[i++] = '/';
				echochar('/');
		
				continue;
			}

			if(i == 1){
				if(buffer[0] == '0' && c == '0') continue;
				if(buffer[0] == '1' && c > '2'){
					buffer[0] = '0';
					buffer[i++] = '1';
					buffer[i++] = '/';
					
					if(c == '3'){
						buffer[i++] = c;
					}
					else{
						buffer[i++] = '0';
						buffer[i++] = c;
						buffer[i++] = '/';
					}

					buffer[i] = 0;
					
					message_out(message);
					printw(buffer);
					
					continue;
				}
			}

			if(i == 4){
				if(buffer[3] == '0' && c == '0') continue;
				if(buffer[3] == '3' && c >  '1') continue;
			}

			buffer[i++] = c;
			echochar(c);

			if(i == 2 || i == 5){
				buffer[i++] = '/';
				echochar('/');
			}

			continue;
		}

		if(c == '/' && (i == 1 || i == 4)){
			if(buffer[i - 1] == '0') continue;
		
			buffer[i] = buffer[i - 1];
			buffer[i - 1] = '0';
			buffer[++i] = '/';
			buffer[++i] = '\0';

			message_out(message);
			printw(buffer);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			if(buffer[i-1] == '/') buffer[--i] = '\0';

			buffer[--i] = '\0';
			message_out(message);
			printw(buffer);
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			buffer[i=0] = '\0';
			message_out(message);
			continue;
		}

		if(c == 3 || c == 7){
			free(buffer);
			return 0;
		}

		if(c == '\t'){
			real_time = time(NULL);
			cur_time = localtime(&real_time);

			if(i == 0){
				buffer[i++] = ((cur_time->tm_mon + 1) / 10) + '0';
				echochar(buffer[i-1]);
				buffer[i++] = ((cur_time->tm_mon + 1) % 10) + '0';
				echochar(buffer[i-1]);
				buffer[i++] = '/';
				echochar(buffer[i-1]);
			}
			else if(i == 3 && buffer[0] == ((cur_time->tm_mon + 1) / 10) + '0' && buffer[1] == ((cur_time->tm_mon + 1) % 10) + '0'){
				buffer[i++] = (cur_time->tm_mday / 10) + '0';
				echochar(buffer[i-1]);
				buffer[i++] = (cur_time->tm_mday % 10) + '0';
				echochar(buffer[i-1]);
				buffer[i++] = '/';
				echochar(buffer[i-1]);
				buffer[i++] = ((cur_time->tm_year % 100) / 10) + '0';
				echochar(buffer[i-1]);
				buffer[i++] = ((cur_time->tm_year % 100) % 10) + '0';
				echochar(buffer[i-1]);
			}
			else ungetch('\n');
			continue;
		}

		if(c == '\n'){
			if(i != max_chars && i != 0 && i != 4 && i != 6){                     /* places enter makes sense */
				c = my_getch();
				if(c != KEY_UP && c != KEY_DOWN && c != KEY_RIGHT && c != KEY_LEFT) ungetch(c);  /* avoid infinitely processing arrow key (see below) */

				continue;
			}

			if (i == 4){
				if(buffer[i - 1] == '0') continue;
		
				buffer[i] = buffer[i - 1];
				buffer[i - 1] = '0';
				buffer[++i] = '/';
				buffer[++i] = '\0';
			}

			/* user left off year, so we'll assume this year (or last, if that makes more sense) */
			if(i == 6){
				int mon, mday;

				real_time = time(NULL);
				cur_time = localtime(&real_time);

				mon = (10 * (buffer[0] - '0')) + (buffer[1] - '0') - 1;
				mday = (10 * (buffer[3] - '0')) + (buffer[4] - '0');
	
				if(mon > cur_time->tm_mon || (mon == cur_time->tm_mon && mday > cur_time->tm_mday)){
					cur_time->tm_year--;
				}

				buffer[i++] = ((cur_time->tm_year % 100) / 10) + '0';
				buffer[i++] = ((cur_time->tm_year % 100) % 10) + '0';
			}

			buffer[i] = '\0';
			strcpy(date, buffer);
			free(buffer);

			timeout(0);
			c = getch();
			if(c == ERR) c = KEY_RIGHT;
			ungetch(c);
			timeout(-1);
	       
			return 1;
		}

		if(c == KEY_RIGHT || c == KEY_LEFT || c == KEY_UP || c == KEY_DOWN){
			ungetch(c);                                                                           /* see this */
			ungetch('\n');
			continue;
		}

		if(c == -1){                     /* message buffer needs to be redrawn */
			message_out(message);
			buffer[i] = '\0';
			printw(buffer);
			continue;
		}
    }
}

int get_money(char* money){
    int c;
    int i;
    int point = -1;
    int max_chars = MONEY_WIDTH;
    char message[] = "Money: \0";

    char* buffer = (char *) malloc(max_chars + 1);

    message_out(message);

    for(i=0; i < max_chars; i++){
		buffer[i] = money[i];
		if(buffer[i] == '\0') break;

		if(point > -1) point++;
		if(buffer[i] == '.') point = 0;
		echochar(buffer[i]);
    }

    for(;;){
		c = my_getch();

		if(c >= '0' && c <= '9' && i < max_chars){
			if(point == 2) continue;             /* a cent is the smallest amount of money */
			if(point == -1 && i >= max_chars - 3) continue;                     /* no room */

			if(point > -1) point++;

			if(i == 0){
				buffer[i++] = '+';
				echochar('+');
			}

			buffer[i++] = c;
			echochar(c);

			/* leading zeroes only make sense in front of a decimal point */
			if(i == 2 && c == '0'){
				buffer[i++] = '.';
				echochar('.');
				point = 0;
				continue;
			}

			continue;
		}

		if(c == '-' && i == 0){
			buffer[i++] = '-';
			echochar('-');
		}

		if(c == '+' && i == 0){
			buffer[i++] = '+';
			echochar('+');
		}

		if(c == '.' && i < max_chars - 2){
			if(point > -1) continue;

			if(i == 0){
				buffer[i++] = '+';
				echochar('+');
			}

			if(i == 1){
				buffer[i++] = '0';
				echochar('0');
			}

			buffer[i++] = '.';
			echochar('.');
			point = 0;
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			if(point > -1) point--;

			buffer[--i] = '\0';

			/* leading zeroes only make sense in front of a decimal point */
			if(i == 2 && buffer[1] == '0') buffer[--i] = '\0';
      
			message_out(message);
			printw(buffer);
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			buffer[i=0] = '\0';
			point = -1;

			message_out(message);
			continue;
		}

		if(c == 3 || c == 7){
			free(buffer);
			return 0;
		}

		if(c == '\n' || c == '\t'){
			if(point == -1 && i > 1){
				buffer[i++] = '.';
				buffer[i++] = '0';
				buffer[i++] = '0';
			}
			else if(point == 0){
				buffer[i++] = '0';
				buffer[i++] = '0';
			}
			else if(point == 1){
				buffer[i++] = '0';
			}

			buffer[i] = '\0';

			strcpy(money, buffer);
			free(buffer);

			timeout(0);
			c = getch();
			if(c == ERR) c = KEY_RIGHT;
			ungetch(c);
			timeout(-1);

			return 1;
		}

		if(c == KEY_LEFT || c == KEY_UP || c == KEY_DOWN || c == KEY_RIGHT){
			ungetch(c);
			ungetch('\n');
			continue;
		}

		if(c == -1){                     /* message buffer needs to be redrawn */
			message_out(message);
			buffer[i] = '\0';
			printw(buffer);
			continue;
		}
    }
}

void get_account_name_switch(char* name){
    int c;
    int i = 0;
    int need_clear = 0;

    clear_line(LINES - 1);
    move(LINES - 1, 0);
    printw("Switch to buffer: (default %s) ", get_last_account_name());
    refresh();

    for(;;){
		c = my_getch();

		if(c == ' ' || c == '\t'){
			int n;
	    
			name[i] = 0;
			n = complete(name);
			if(i == 0 && n != 1) continue;

			for(i = 0; name[i] != 0; i++) ;

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Switch to buffer: (default %s) ", get_last_account_name());
			printw(name);

			if(n == 0) printw(" [no matches]");
			else if(n == 1) printw(" [complete]");
			else if(n > 1 && whole_namep(name)) printw(" [non-unique]");
			else printw(" [incomplete]");

			need_clear = 1;

			move(LINES - 1, 0);
			printw("Switch to buffer: (default %s) ", get_last_account_name());
			printw(name);
	    
			refresh();
			continue;
		}

		if(c >= 32 && c <= 126 && i < ACCOUNT_NAME_MAX_LENGTH){
			if(need_clear){
				clear_line(LINES - 1);
				move(LINES - 1, 0);
				printw("Switch to buffer: (default %s) ", get_last_account_name());
				name[i]=0;
				printw(name);

				refresh();
			}
	    
			name[i++] = c;
			echochar(c);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			name[--i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Switch to buffer: (default %s) ", get_last_account_name());
			printw(name);

			refresh();
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			name[i=0] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Switch to buffer: (default %s) ", get_last_account_name());
			refresh();

			continue;
		}

		if(c == 3 || c == 7){
			clear_line(LINES - 1);
			refresh();
	    
			name[0] = '\0';
			return;
		}

		if(c == '\n'){
			if(i == 0) strncpy(name, get_last_account_name(), ACCOUNT_NAME_MAX_LENGTH);
			else name[i] = '\0';
	    
			return;
		}

		if(c == -1){                     /* message buffer needs to be redrawn */
			name[i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
	    
			printw("Switch to buffer: (default %s) ", get_last_account_name());
			printw(name);
	    
			refresh();

			continue;
		}
    }
}

void get_account_name_kill(char* name){
    int c;
    int i = 0;
    int need_clear = 0;

    clear_line(LINES - 1);
    move(LINES - 1, 0);
    printw("Kill buffer: (default %s) ", current_account->account_name);
    refresh();

    for(;;){
		c = my_getch();

		if(c == ' ' || c == '\t'){
			int n;
	    
			name[i] = 0;
			n = complete(name);
			if(i == 0 && n != 1) continue;

			for(i = 0; name[i] != 0; i++) ;

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Kill buffer: (default %s) ", current_account->account_name);
			printw(name);

			if(n == 0) printw(" [no matches]");
			else if(n == 1) printw(" [complete]");
			else if(n > 1 && whole_namep(name)) printw(" [non-unique]");
			else printw(" [incomplete]");

			need_clear = 1;

			move(LINES - 1, 0);
			printw("Kill buffer: (default %s) ", current_account->account_name);
			printw(name);
	    
			refresh();
			continue;
		}

		if(c >= 32 && c <= 126 && i < ACCOUNT_NAME_MAX_LENGTH){
			if(need_clear){
				clear_line(LINES - 1);
				move(LINES - 1, 0);
				printw("Kill buffer: (default %s) ", current_account->account_name);
				name[i]=0;
				printw(name);

				refresh();
			}
	    
			name[i++] = c;
			echochar(c);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			name[--i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Kill buffer: (default %s) ", current_account->account_name);
			printw(name);

			refresh();
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			name[i=0] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			printw("Kill buffer: (default %s) ", current_account->account_name);
			refresh();

			continue;
		}

		if(c == 3 || c == 7){
			clear_line(LINES - 1);
			refresh();
	    
			name[0] = '\0';
			return;
		}

		if(c == '\n'){
			if(i == 0) strncpy(name, current_account->account_name, ACCOUNT_NAME_MAX_LENGTH);
			else name[i] = '\0';

			return;
		}

		if(c == -1){                          /* message buffer needs to be redrawn */
			name[i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
	    
			printw("Kill buffer: (default %s) ", current_account->account_name);
			printw(name);
	    
			refresh();

			continue;
		}
    }
}

void get_file_name_write(char* name){
    int c;
    int i = 0;
    int need_clear = 0;

    clear_line(LINES - 1);
    move(LINES - 1, 0);
    if(current_account->last_file[0]) printw("Write to file: (default %s) ", current_account->last_file);
    else printw("Write to file: (default %s) ", current_account->account_name);
    refresh();

    for(;;){
		c = my_getch();

		if(c == ' ' || c == '\t'){
			/* completion? */
			continue;
		}

		if(c >= 32 && c <= 126 && i < FILE_NAME_MAX_LENGTH){
			if(need_clear){
				clear_line(LINES - 1);
				move(LINES - 1, 0);
				if(current_account->last_file[0]) printw("Write to file: (default %s) ", current_account->last_file);
				else printw("Write to file: (default %s) ", current_account->account_name);
				name[i]=0;
				printw(name);

				refresh();
			}

			name[i++] = c;
			echochar(c);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			name[--i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			if(current_account->last_file[0]) printw("Write to file: (default %s) ", current_account->last_file);
			else printw("Write to file: (default %s) ", current_account->account_name);
			printw(name);

			refresh();
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			name[i=0] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
			if(current_account->last_file[0]) printw("Write to file: (default %s) ", current_account->last_file);
			else printw("Write to file: (default %s) ", current_account->account_name);
			refresh();

			continue;
		}

		if(c == 3 || c == 7){
			clear_line(LINES - 1);
			refresh();
	    
			name[0] = '\0';
			return;
		}

		if(c == '\n'){
			if(i == 0){
				if(current_account->last_file[0]) strncpy(name, current_account->last_file, ACCOUNT_NAME_MAX_LENGTH);
				else strncpy(name, current_account->account_name, ACCOUNT_NAME_MAX_LENGTH);
			}

			else name[i] = '\0';

			return;
		}

		if(c == -1){                      /* message buffer needs to be redrawn */
			name[i] = '\0';

			clear_line(LINES - 1);
			move(LINES - 1, 0);
	    
			if(current_account->last_file[0]) printw("Write to file: (default %s) ", current_account->last_file);
			else printw("Write to file: (default %s) ", current_account->account_name);

			printw(name);
	    
			refresh();

			continue;
		}

    }
}
void get_file_name_read(char* name){
    int c;
    int i = 0;
    int need_clear = 0;

    message_out("Read from file: ");

    for(;;){
		c = my_getch();

		if(c == ' ' || c == '\t'){
			/* completion? */
			continue;
		}

		if(c >= 32 && c <= 126 && i < FILE_NAME_MAX_LENGTH){
			if(need_clear){
				message_out("Read from file: ");

				name[i]=0;
				printw(name);

				refresh();
			}
	    
			name[i++] = c;
			echochar(c);
			continue;
		}

		if(c == 8 && i > 0){             /* backspace */
			name[--i] = '\0';

			message_out("Read from file: ");

			printw(name);

			refresh();
			continue;
		}

		if(c == 21 && i > 0){            /* ^U */
			name[i=0] = '\0';

			message_out("Read from file: ");

			continue;
		}

		if(c == 3 || c == 7){
			clear_line(LINES - 1);
			refresh();
	    
			name[0] = '\0';
			return;
		}

		if(c == '\n'){
			name[i] = '\0';

			return;
		}

		if(c == -1){                      /* message buffer needs to be redrawn */
			name[i] = '\0';

			message_out("Read from file: ");

			printw(name);
	    
			refresh();

			continue;
		}

    }
}

int message_out_get_yes_no(char* message, int default_ans){
    int c, c2;
    int ret;

    message_out(message);

    for(ret = 0; ret == 0;){
		c = my_getch();

		if(c == '\n') ret = default_ans;
		if(c == 3 || c == 7) ret = 'n';
		c2 = tolower(c);
		if(c2 == 'y' || c2 == 'n') ret = c2;
		if(c == -1) message_out(message);
    }

    clear_line(LINES - 1);
    refresh();

    return ret;
}

int file_exists(char* name){
    struct stat buf;
    errno = 0;

    stat(name, &buf);

    if(errno != ENOENT) return 1;

    return 0;
}
