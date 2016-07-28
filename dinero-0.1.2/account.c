/* account.c */

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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#include "account.h"


typedef struct ACCOUNT_NODE{
    entry* Entry;
     
    struct ACCOUNT_NODE* next;
    struct ACCOUNT_NODE* prev;
}account_node;

struct ACCOUNT{
    account_node* head;
    account_node* tail;
     
    account_node* cur_node;
    int cur_node_num;

    int bottom_entry_num;
     
    char balance[20];
};


static account_node* locate_node(account* A, int location);
static int insert_node(account* A, int location);
static int delete_node(account* A, int location);

static void add_to_account_balance(account* A, char* ammount);


entry* new_entry(){
    entry* e = (entry *) malloc(sizeof(entry));

    e->Date[0] = e->Comment[0] = e->Money[0] = '\0';

    return e;
}

account* new_account(){
    account* A = (account *) malloc(sizeof(account));
     
    A->head = NULL;
    A->tail = NULL;
     
    A->cur_node = NULL;
    A->cur_node_num = -1;

    A->bottom_entry_num = -1;

    strcpy(A->balance, "+0.00");

    return A;
}

int insert_entry(account* A, int location, entry* Entry){
    int err = insert_node(A, location);

    if(err != 0) return err;

    A->cur_node->Entry = Entry;

    if(Entry != NULL && Entry->Money[0] != '\0'){
	add_to_account_balance(A, Entry->Money);
    }

    return 0;
}

int delete_entry(account* A, int location){
    account_node* an;

    an = locate_node(A, location);

    if(an != NULL && an->Entry != NULL && an->Entry->Money[0] != '\0'){
	if(an->Entry->Money[0] == '+') an->Entry->Money[0] = '-';
	else an->Entry->Money[0] = '+';

	add_to_account_balance(A, an->Entry->Money);
    }

    return delete_node(A, location);
}

entry* get_entry(account* A, int location){
    account_node* an = locate_node(A, location);

    if(an == NULL) return NULL;

    A->cur_node = an;
    A->cur_node_num = location;

    return an->Entry;
}

void set_entry(account* A, int location, entry* Entry){
    account_node* an = locate_node(A, location);

    if(Entry == NULL && location == A->bottom_entry_num){
	delete_entry(A, location);
	return;
    }

    if(an == NULL){
	insert_entry(A, location, Entry);
	return;
    }

    if(an->Entry == NULL){
	an->Entry = new_entry();
    }

    if(Entry == NULL || strcmp(an->Entry->Money, Entry->Money)){
	if(an->Entry->Money[0] == '+') an->Entry->Money[0] = '-';
	else an->Entry->Money[0] = '+';

	add_to_account_balance(A, an->Entry->Money);
	if(Entry) add_to_account_balance(A, Entry->Money);
    }
    
    an->Entry = Entry;
}

int get_bottom_entry_num(account* A){
    if(A->bottom_entry_num == -1) return 0;
    return A->bottom_entry_num;
}

int account_is_empty(account* A){
    if(A->bottom_entry_num == -1) return 1;

    return 0;
}

char* get_balance(account* A){
    return A->balance;
}

static account_node* locate_node(account* A, int location){
    if(A == NULL || location < 0 || location > A->bottom_entry_num) return NULL;

    if(location == 0) return A->head;
    if(location == A->bottom_entry_num) return A->tail;

    if(location == A->cur_node_num) return A->cur_node;

    if(location > A->cur_node_num){
	account_node* an = A->cur_node;
	int i = A->cur_node_num;

	for(;;an = an->next, i++) if(i == location) return an;
    }

    if(location < A->cur_node_num){
	account_node* an = A->cur_node;
	int i = A->cur_node_num;

	for(;;an = an->prev, i--) if(i == location) return an;
    }

    return NULL;                                       /* for -Wall */
}

static int insert_node(account* A, int location){
    if(A == NULL) return 1;

    if(location == 0){
	account_node* an = A->head;

	A->head = (account_node *) malloc(sizeof(account_node));

	A->head->prev = NULL;
	A->head->next = an;

	if(an != NULL) an->prev = A->head;
	else A->tail = A->head;        /* head was NULL, so this is the only node */

	A->cur_node = A->head;
	A->cur_node_num = 0;
	A->bottom_entry_num++;

	return 0;
    }

    else if(location == A->bottom_entry_num + 1){
	account_node* an = A->tail;
	  
	A->tail = (account_node *) malloc(sizeof(account_node));

	A->tail->next = NULL;
	A->tail->prev = an;

	an->next = A->tail;

	A->cur_node = A->tail;
	A->cur_node_num = ++(A->bottom_entry_num);

	return 0;
    }

    else if(location > A->bottom_entry_num + 1){
	account_node* an;

	insert_node(A, location - 1);
	A->cur_node->Entry = NULL;

	an = A->tail;

	A->tail = (account_node *) malloc(sizeof(account_node));

	A->tail->next = NULL;
	A->tail->prev = an;

	an->next = A->tail;

	A->cur_node = A->tail;
	A->cur_node_num = ++(A->bottom_entry_num);

	return 0;
    }

    else{
	account_node* old_an;
	account_node* new_an;

	if(location < 0) return 1;
	  
	old_an = locate_node(A, location);
	new_an = (account_node *) malloc(sizeof(account_node));

	new_an->next = old_an;
	new_an->prev = old_an->prev;
	old_an->prev = new_an;
	new_an->prev->next = new_an;

	A->cur_node = new_an;
	A->cur_node_num = location;
	A->bottom_entry_num++;
	  
	return 0;
    }
}

static int delete_node(account* A, int location){
    account_node* an = locate_node(A, location);

    if(an == NULL) return 1;

    if(location == A->bottom_entry_num && an->prev != NULL && an->prev->Entry == NULL){
	an->prev->next = NULL;
	A->tail = an->prev;
	A->bottom_entry_num--;

	delete_node(A, location - 1);

	free(an->Entry);
	free(an);

	return 0;
    }

    if(an->prev != NULL) an->prev->next = an->next;

    if(an->next != NULL){
	an->next->prev = an->prev;

	A->cur_node = an->next;
	A->cur_node_num = location;
    }

    else{
	A->cur_node = an->prev;
	A->cur_node_num = location - 1;
    }

    A->bottom_entry_num--;

    free(an->Entry);

    free(an);

    if(location == 0) A->head = A->cur_node;
    if(location == A->bottom_entry_num + 1) A->tail = A->cur_node;

    return 0;
}

static void add_to_account_balance(account* A, char* ammount){
    double dbal = atof(A->balance);

    dbal += atof(ammount);

    sprintf(A->balance, "%+.2f", dbal);
}

/* entries are preceded by difference from last line rather
 * than literal line number so that files can be concatenated
 * at the command line.  while i doubt there will ever be a
 * reason to use this feature, it just seems like a better
 * file format if you *could* do that if you *wanted* to.
 *
 * (it should also shave a few percent off the file size in
 *  most cases, but that's not really a factor in my decision)
 */
int write_account_to_file(account* A, char* file_name){
    FILE* fp;
    account_node* an;
    int i, j;

    if(!A) return 2;

    an = A->head;

    fp = fopen(file_name, "w");

    if(!fp) return 1;

    for(i = 0, j = -1; i <= A->bottom_entry_num; i++, an = an->next){
	if(an->Entry){
	    fprintf(fp, "%d\n%s\n%s\n%s\n", i - j, an->Entry->Date, an->Entry->Comment, an->Entry->Money);

	    j = i;
	}
    }

    fclose(fp);

    return 0;
}

int read_account_from_file(account* A, char* file_name){
    FILE* fp;
    entry* Entry;
    char buf[MAX_FIELD_WIDTH + 1];
    int i, tmp;

    if(!A) return 2;

    fp = fopen(file_name, "r");

    if(!fp) return 1;

    for(i = -1, Entry = new_entry(); ; Entry = new_entry()){
	if(! fgets(buf, MAX_FIELD_WIDTH + 1, fp)){
	    free(Entry);

	    fclose(fp);
	    return 0;
	    break;
	}

	tmp = atoi(buf);
	if(tmp < 1){
	    free(Entry);

	    fclose(fp);
	    return -1;
	    break;
	}

	i += tmp;

	if(! fgets(buf, MAX_FIELD_WIDTH + 1, fp)){
	    free(Entry);

	    fclose(fp);
	    return -1;
	    break;
	}

	buf[strlen(buf) - 1] = '\0';                               /* get rid of '\n' */
	strncpy(Entry->Date, buf, DATE_WIDTH + 1);

	if(! fgets(buf, MAX_FIELD_WIDTH + 1, fp)){
	    free(Entry);

	    fclose(fp);
	    return -1;
	    break;
	}

	buf[strlen(buf) - 1] = '\0';                               /* get rid of '\n' */
	strncpy(Entry->Comment, buf, COMMENT_WIDTH + 1);

	if(! fgets(buf, MAX_FIELD_WIDTH + 1, fp)){
	    free(Entry);

	    fclose(fp);
	    return -1;
	    break;
	}

	buf[strlen(buf) - 1] = '\0';                               /* get rid of '\n' */
	strncpy(Entry->Money, buf, MONEY_WIDTH + 1);

	set_entry(A, i, Entry);
    }

    fclose(fp);

    return 0;
}
