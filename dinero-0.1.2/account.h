/* account.h */

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

#ifndef ACCOUNT_H
#define ACCOUNT_H

#define DATE_WIDTH 8
#define MONEY_WIDTH 10
#define COMMENT_WIDTH 100
#define MAX_FIELD_WIDTH COMMENT_WIDTH

typedef struct{
    char Date[DATE_WIDTH + 1];
    char Money[MONEY_WIDTH + 1];
    char Comment[COMMENT_WIDTH + 1];
}entry;

typedef struct ACCOUNT account;


entry* new_entry();
account* new_account();

int insert_entry(account* A, int location, entry* Entry);
int delete_entry(account* A, int location);

entry* get_entry(account* A, int location);
void set_entry(account* A, int location, entry* Entry);
int get_bottom_entry_num(account* A);
int account_is_empty(account* A);

char* get_balance(account* A);

int write_account_to_file(account* A, char* file_name);
int read_account_from_file(account* A, char* file_name);

#endif   /* ACCOUNT_H */
