/* dinero.c */

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
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "dinero.h"
#include "io.h"
#include "account.h"
#include "session.h"


static void edit_field(int c);
static int open_up(char* name);
static void quick_save();
static void save();
static void delete_line();
static void empty_field();
static int quit();
static int kill_okay(char* name);


void start_screen(int num_of_files, char** file_names){
    initscr(); 
    keypad(stdscr, TRUE);
    cbreak();
    noecho();
    raw();

    old_sig_winch = signal(SIGWINCH, sig_winch_handle);

    if(num_of_files == 0){
	add_new_account_to_session("new");

	update_screen();
	my_refresh();
    }
    else{
	int none_exist = 1;

	for(; num_of_files > 0; --num_of_files, ++file_names){
	    if(open_up(*file_names) == 0)
		none_exist = 0;
	}

	if(none_exist){
	    add_new_account_to_session("new");

	    update_screen();
	    my_refresh();
	}
	
    }
    
    sig_winch();    /* complains if terminal is too small */
}

void end_screen(){
    endwin();
}

/* returns last part of path */
char* extract_last_field(char* path){
    int i = strlen(path) - 1;

    for(path += i; i > 0; --i, --path)
	if(*path == '/'){
	    ++path;
	    break;
	}

    return path;
}

void process_input(){
    int key = 0;

    for(;;){
	key = my_getch();

	clear_line(LINES - 1);                        /* clear message buffer */
	refresh();

	if(key == KEY_DOWN){
	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, 0);

	    if(current_account->cur_line - current_account->line_offset == LINES - 3){
		current_account->line_offset++;
		current_account->cur_line++;

		update_screen();
		my_refresh();
		continue;
	    }

	    /* else */

	    current_account->cur_line++;

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);
	    my_refresh();
	    continue;
	}

	if(key == KEY_UP && current_account->cur_line > 0){
	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, 0);

	    if(current_account->cur_line == current_account->line_offset){
		current_account->line_offset--;
		current_account->cur_line --;

		update_screen();
		my_refresh();
		continue;
	    }

	    /* else */

	    current_account->cur_line --;

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);

	    my_refresh();
	    continue;
	}

	if(key == KEY_RIGHT){
	    if(current_account->cur_col < 2){
		set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col++, 0);
		set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);

		my_refresh();
		continue;
	    }

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, 0);

	    current_account->cur_col = 0;
	       
	    if(current_account->cur_line - current_account->line_offset == LINES - 3){
		current_account->line_offset++;
		current_account->cur_line++;

		update_screen();
		my_refresh();
		continue;
	    }

	    /* else */

	    current_account->cur_line++;

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);
	    my_refresh();
	    continue;
	}

	if(key == KEY_LEFT){
	    if(current_account->cur_col > 0){
		set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col--, 0);
		set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);

		my_refresh();
		continue;
	    }

	    if(current_account->cur_line == 0) continue;

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, 0);

	    current_account->cur_col = 2;

	    if(current_account->cur_line == current_account->line_offset){
		current_account->line_offset--;
		current_account->cur_line --;

		update_screen();
		my_refresh();
		continue;
	    }

	    /* else */

	    current_account->cur_line --;

	    set_field_attrib(current_account->cur_line - current_account->line_offset, current_account->cur_col, A_REVERSE);

	    my_refresh();
	    continue;
	}

	if(key == KEY_HOME){
	    current_account->cur_line = current_account->line_offset = 0;

	    update_screen();
	    my_refresh();
	    continue;
	}

	if(key == KEY_END){
	    current_account->cur_line = get_bottom_entry_num(current_account->Account);

	    if(current_account->cur_line - current_account->line_offset > LINES - 3) current_account->line_offset = current_account->cur_line - (LINES - 3);
	    if(current_account->cur_line < current_account->line_offset) current_account->line_offset = current_account->cur_line;
      
	    update_screen();
	    my_refresh();
	    continue;
	}

	if(key == KEY_PPAGE && current_account->cur_line > 0){
	    if(current_account->line_offset > LINES - 3){
		current_account->line_offset -= LINES - 3;
		current_account->cur_line -= LINES - 3;
	    }

	    else{
		current_account->line_offset = 0;
		if(current_account->cur_line <= LINES - 3) current_account->cur_line = 0;
		else current_account->cur_line -= LINES - 3;
	    }

	    update_screen();
	    my_refresh();
	    continue;
	}

	if(key == KEY_NPAGE){
	    current_account->line_offset += LINES - 3;
	    current_account->cur_line += LINES - 3;

	    update_screen();
	    my_refresh();
	    continue;
	}

	if(key == 11){                                               /* ^K : kill line */
	    if(current_account->cur_line > get_bottom_entry_num(current_account->Account)) continue;
	    delete_line();
	    continue;
	}

	if(key == 5){                                                /* ^E : empty field */
	    empty_field();
	    continue;
	}

	if(key == 15){                                               /* ^O : insert blank line */
	    if(current_account->cur_line > get_bottom_entry_num(current_account->Account)) continue;
	    if(current_account->cur_line == get_bottom_entry_num(current_account->Account) &&
	       ! get_entry(current_account->Account, current_account->cur_line))
		continue;
	    
	    insert_entry(current_account->Account, current_account->cur_line, NULL);
	    current_account->modified = 1;

	    update_screen();
	    my_refresh();
	    continue;
	}

	if(key == 24){
	    key = finish_ctrl_x();

	    if(key == 3){                                            /* ^C */
		if(quit()) return;

		clear_line(LINES - 1);
		refresh();
		continue;
	    }

	    if(key == 7){                                            /* ^G */
		clear_line(LINES - 1);
		refresh();
		continue;

	    }

	    if(key == 6){                                            /* ^F */
		open_up(NULL);

		continue;
	    }

	    if(key == 19){                                           /* ^S */
		if(current_account->modified){
		    quick_save();
		    continue;
		}

		if(message_out_get_yes_no("No changes need saving - force save anyway? [y/N] ", 'n') == 'y'){
		    clear_line(LINES - 1);
		    my_refresh();

		    quick_save();
		    continue;
		}

		clear_line(LINES - 1);
		my_refresh();

		continue;
	    }

	    if(key == 23){                                           /* ^W */
		if(current_account->modified){
		    save();
		    continue;
		}

		if(message_out_get_yes_no("No changes need saving - force save anyway? [y/N] ", 'n') == 'y'){
		    clear_line(LINES - 1);
		    my_refresh();

		    save();
		    continue;
		}

		clear_line(LINES - 1);
		my_refresh();

		continue;
	    }

	    if(key == 's'){
		if(any_modified_accounts() || message_out_get_yes_no("No changes need saving - force save anyway? [y/N] ", 'n') == 'y'){
		    int i = write_all();

		    clear_line(LINES - 1);
		    move(LINES - 1, 0);
		    if(i == 1) printw("Wrote 1 file");
		    else printw("Wrote %d files", i);
		    my_refresh();
		}

		else{                                      /* in case they said 'n' above */
		    clear_line(LINES - 1);
		    refresh();
		}

		continue;

	    }

	    if(key == 'b'){
		char* name = calloc(ACCOUNT_NAME_MAX_LENGTH, 1);
		
		get_account_name_switch(name);
		
		if(name[0]){
		    if(get_account(name)){
			switch_to_account(name);
		    }

		    else{
			add_new_account_to_session(name);
		    }

		    update_screen();
		    my_refresh();
		}

		free(name);
		continue;
	    }

	    if(key == 'k'){
		char* name = calloc(ACCOUNT_NAME_MAX_LENGTH, 1);
		
		get_account_name_kill(name);

		if(! name[0]){
		    free(name);
		    continue;
		}

		if(! get_account(name)){
		    char message[80];

		    sprintf(message, "Can not kill nonexistent buffer \"%s\"", name);

		    message_out(message);
		    my_refresh();

		    free(name);
		    continue;
		}

		if(name[0] && kill_okay(name)){
		    kill_account(name);

		    update_screen();
		    my_refresh();
		    continue;
		}

		free(name);
		continue;
	    }

	    clear_line(LINES - 1);
	    my_refresh();
	}

	else{
	    edit_field(key);
	    continue;
	}
	  
    }

}

static void edit_field(int c){
    int max_width;
    entry* temp;
    entry* old_entry = get_entry(current_account->Account, current_account->cur_line);

    if(current_account->cur_col == 0){ 
	if(old_entry == NULL || old_entry->Date[0] == '\0'){
	    if((c < '0' || c > '9') && c != '\t' && c != '\n' && c != 't' && c != 'y') return;
	}
	else if(c != '\n' && c != 8) return;
    }
    else if(current_account->cur_col == 1){ 
	if(old_entry == NULL || old_entry->Comment[0] == '\0'){
	    if((c < 32 || c > 126) && c != '\n') return;
	}
	else if(c != '\n' && c != 8) return;
    }
    else if(current_account->cur_col == 2){ 
	if(old_entry == NULL || old_entry->Money[0] == '\0'){
	    if((c < '0' || c > '9') && c != '-' && c != '+' && c != '.' && c != '\n') return;
	}
	else if(c != '\n' && c != 8) return;
    }

    if(c != '\n') ungetch(c);

    temp = new_entry();

    if(old_entry != NULL){
	strncpy(temp->Date, old_entry->Date, DATE_WIDTH + 1);

	max_width = min(COLS - DATE_WIDTH - MONEY_WIDTH - 1, COMMENT_WIDTH);
	strncpy(temp->Comment, old_entry->Comment, max_width);
	temp->Comment[max_width + 1] = '\0';

	strncpy(temp->Money, old_entry->Money, MONEY_WIDTH + 1);
    }
  
    if(current_account->cur_col == 0){
	if(get_date(temp->Date)){
	    if(old_entry == NULL){
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    else{
		free(old_entry);
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    current_account->modified = 1;
	}
	else free(temp);
    }
    else if(current_account->cur_col == 1){
	if(get_comment(temp->Comment)){
	    if(old_entry == NULL){
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    else{
		free(old_entry);
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    current_account->modified = 1;
	}
	else free(temp);
    }
    else if(current_account->cur_col == 2){
	if(get_money(temp->Money)){
	    if(old_entry == NULL){
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    else{
		free(old_entry);
		set_entry(current_account->Account, current_account->cur_line, temp);
	    }
	    current_account->modified = 1;
	}
	else free(temp);
    }

    update_screen();
    my_refresh();
}

static int open_up(char* name){
    al_entry* ale;
    int ret;

    if(! name){
	char buf[FILE_NAME_MAX_LENGTH];
	name = buf;

	get_file_name_read(name);

	clear_line(LINES - 1);

	if(! name[0]){
	    return 2;
	}

	if(! strncmp(current_account->last_file, name, FILE_NAME_MAX_LENGTH)){
	    return 0;
	}

	ale = file_is_open(name);
	if(ale){
	    switch_to_account(ale->account_name);

	    update_screen();
	    my_refresh();

	    return 0;
	}

	if(! file_exists(name)){
	    char message[FILE_NAME_MAX_LENGTH + 32];
	    sprintf(message, "File \"%s\" does not exist!", name);
	    
	    message_out(message);
	    my_refresh();

	    return 1;
	}

	if(! account_is_empty(current_account->Account)){
	    add_new_account_to_session(extract_last_field(name));
	}
	else{
	    char buf[ACCOUNT_NAME_MAX_LENGTH];
	    char* bufp = buf;

	    strncpy(bufp, extract_last_field(name), ACCOUNT_NAME_MAX_LENGTH);
	    adjust_name(bufp);

	    if(strncmp(current_account->account_name, bufp, ACCOUNT_NAME_MAX_LENGTH)){
		strncpy(current_account->account_name, bufp, FILE_NAME_MAX_LENGTH);

		my_refresh();
	    }
	}
    }
    else{
	if(! file_exists(name)){
	    return 1;
	}

	if(file_is_open(name)){
	    return 0;
	}

	add_new_account_to_session(extract_last_field(name));
    }

    ret = read_account_from_file(current_account->Account, name);
    if(ret < 1){
	strncpy(current_account->last_file, name, FILE_NAME_MAX_LENGTH);
    }
    else{
	kill_account(current_account->account_name);
    }

    update_screen();
    if(ret == -1) message_out("File damaged!");
    my_refresh();

    return 0;
}

static void quick_save(){
    if (current_account->last_file[0]){
	char* name = current_account->last_file;
	int ret;

	ret = write_account_to_file(current_account->Account, name);

	if(ret == 0){
	    char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	    sprintf(message, "Wrote \"%s\"", name);

	    message_out(message);
	    my_refresh();

	    free(message);

	    current_account->modified = 0;

	    strncpy(current_account->last_file, name, FILE_NAME_MAX_LENGTH);

	    if(strncmp(current_account->account_name, extract_last_field(name), ACCOUNT_NAME_MAX_LENGTH)){
		adjust_name(name);
		strncpy(current_account->account_name, extract_last_field(name), FILE_NAME_MAX_LENGTH);
	    }
	    my_refresh();
	}
    
	else if(ret == 1){
	    char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	    sprintf(message, "Cannot open file \"%s\" for writing!", name);

	    message_out(message);
	    my_refresh();

	    free(message);
	}
    }

    else{
	save();
	return;
    }
}

static void save(){
    char name_buf [FILE_NAME_MAX_LENGTH];
    char* name = name_buf;
    int ret;

    get_file_name_write(name);
    clear_line(LINES - 1);
    
    if(! name[0]){
	return;
    }

    if(strncmp(current_account->last_file, name, FILE_NAME_MAX_LENGTH)){
	if(file_is_open(name)){
	    char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	    sprintf(message, "File \"%s\" already open in another buffer!", name);

	    message_out(message);
	    my_refresh();

	    free(message);
	    return;
	}

	if(file_exists(name)){
	    int c;
	    char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	    sprintf(message, "File \"%s\" already exists! Overwrite? [y/N] ", name);

	    c = message_out_get_yes_no(message, 'n');

	    clear_line(LINES - 1);
	    my_refresh();

	    if(c == 'n'){
		free(message);
		return;
	    }

	    free(message);
	}
    }

    ret = write_account_to_file(current_account->Account, name);
    if(ret == 0){
	char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	sprintf(message, "Wrote \"%s\"", name);

	message_out(message);
	my_refresh();

	free(message);

	current_account->modified = 0;

	strncpy(current_account->last_file, name, FILE_NAME_MAX_LENGTH);

	if(strncmp(current_account->account_name, extract_last_field(name), ACCOUNT_NAME_MAX_LENGTH)){
	    adjust_name(name);
	    strncpy(current_account->account_name, extract_last_field(name), FILE_NAME_MAX_LENGTH);
	}
	my_refresh();
    }
    else if(ret == 1){
	char* message = malloc(FILE_NAME_MAX_LENGTH + 64);
	sprintf(message, "Cannot open file \"%s\" for writing!", name);

	message_out(message);
	my_refresh();

	free(message);
    }
}

static void delete_line(){
    int c;
    entry* old_entry = get_entry(current_account->Account, current_account->cur_line);

    if(old_entry == NULL){                                                          /* line is blank so no need to verify */
	if(get_bottom_entry_num(current_account->Account) == 0) return;
	
	delete_entry(current_account->Account, current_account->cur_line);
	current_account->modified = 1;

	update_screen();
	my_refresh();
	
	return;
    }

    c = message_out_get_yes_no("Really delete line? [Y/n] ", 'y');

    if(c == 'n'){
	clear_line(LINES - 1);
	my_refresh();
	return;
    }
  
    /* c == 'y' */
  
    delete_entry(current_account->Account, current_account->cur_line);
    current_account->modified = 1;

    update_screen();
    clear_line(LINES - 1);
    my_refresh();
}

static void empty_field(){
    int c;
    int col = current_account->cur_col;
    entry* old_entry = get_entry(current_account->Account, current_account->cur_line);

    if(old_entry == NULL) return;
    if(col == 0 && old_entry->Date[0] == '\0') return;
    if(col == 1 && old_entry->Comment[0] == '\0') return;
    if(col == 2 && old_entry->Money[0] == '\0') return;

    c = message_out_get_yes_no("Really clear field? [Y/n] ", 'y');

    if(c == 'n'){
	clear_line(LINES - 1);
	my_refresh();
	return;
    }
  
    /* c == 'y' */
  
    if(col == 0 && (old_entry->Comment[0] || old_entry->Money[0])) old_entry->Date[0] = '\0';
    else if(col == 1 && (old_entry->Date[0] || old_entry->Money[0])) old_entry->Comment[0] = '\0';
    else if(col == 2 && (old_entry->Comment[0] || old_entry->Date[0])) old_entry->Money[0] = '\0';
    else set_entry(current_account->Account, current_account->cur_line, NULL);

    current_account->modified = 1;
    
    update_screen();
    clear_line(LINES - 1);
    my_refresh();
}

static int quit(){

    if(any_modified_accounts()){
	int c;

	c = message_out_get_yes_no("Modified accounts exist!  Really quit? [y/N] ", 'n');

	if(c == 'y') return 1;

	/* c == 'n' */

	clear_line(LINES - 1);
	my_refresh();

	return 0;
    }

    return 1;
}

static int kill_okay(char* name){
    int c;
    
    if(! get_account(name)->modified) return 1;

    c = message_out_get_yes_no("Account modified!  Kill anyway? [y/N]", 'n');

    if(c == 'y') return 1;

    return 0;
}
