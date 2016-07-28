/* session.h */

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

#include "account.h"


#define FILE_NAME_MAX_LENGTH 256
#define ACCOUNT_NAME_MAX_LENGTH FILE_NAME_MAX_LENGTH

typedef struct ACCOUNT_LIST_ENTRY{
    account* Account;

    int cur_line;
    int cur_col;
    int line_offset;

    int modified;
    char last_file[FILE_NAME_MAX_LENGTH];          /* name of file account was last read from or written to    */

    char account_name[ACCOUNT_NAME_MAX_LENGTH];    /* name of account used for switching between open accounts */
}al_entry;

extern al_entry* current_account;

int add_new_account_to_session(char* account_name);
al_entry* get_account(char* account_name);
void switch_to_account(char* account_name);
void kill_account(char* account_name);
char* get_last_account_name();
int complete(char* name);
int whole_namep(char* name);
void adjust_name(char* account_name);


int any_modified_accounts();
al_entry* file_is_open(char* name);

int write_all();
