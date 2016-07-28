/* io.h */

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

#define min(a, b) ((a) < (b) ? (a) : (b))

#undef KEY_RESIZE
#define KEY_RESIZE      0632

void (*old_sig_winch)(int);

void my_refresh();
void clear_line(int line);
void update_screen();
void sig_winch_handle(int sig_num);
void sig_winch();
void set_field_attrib(int y, int x, int attrib);
int my_getch();
int finish_ctrl_x();

void message_out(char* message);
int get_comment(char* comment);
int get_date(char* date);
int get_money(char* money);

void get_account_name_switch(char* name);
void get_account_name_kill(char* name);
void get_file_name_write(char* name);
void get_file_name_read(char* name);
int message_out_get_yes_no(char* message, int default_ans);

int file_exists(char* name);
