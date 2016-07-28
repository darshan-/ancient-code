/* session.c */

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
#include <stdio.h>
#include <string.h>
#include <curses.h>

#include "session.h"
#include "account.h"


typedef struct ACCOUNT_LIST_NODE{
    al_entry* Entry;

    struct ACCOUNT_LIST_NODE* next;
    struct ACCOUNT_LIST_NODE* prev;
}al_node;

al_entry* current_account;
static al_node* current_account_node;
static al_node* head;

static int new_num;

static al_node* get_account_node(char* account_name);
static int num_of_matches(char* name);
static void get_only_match(char* name);
static char* get_first_match(char* name);
static void fill_next_letter_from_first_match(char* name);


int add_new_account_to_session(char* account_name){
    char writable_string[ACCOUNT_NAME_MAX_LENGTH];
    al_node* new_node = malloc(sizeof(al_node));

    new_node->Entry = malloc(sizeof(al_entry));

    strncpy(writable_string, account_name, ACCOUNT_NAME_MAX_LENGTH);


    if(head){                                           /* insert at tail */
	al_node* aln;

	for(aln = head; aln->next != NULL; aln = aln->next) ;  /* get tail */

	new_node->next = NULL;
	new_node->prev = aln;
	aln->next = new_node;
    }

    else{                                               /* insert at head */
	new_node->next = new_node->prev = NULL;

	head = new_node;
    }

    new_node->Entry->Account = new_account();
    new_node->Entry->cur_line = 0;
    new_node->Entry->cur_col = 0;
    new_node->Entry->line_offset = 0;
    new_node->Entry->modified = 0;
    new_node->Entry->last_file[0] = '\0';

    adjust_name(writable_string);
    strncpy(new_node->Entry->account_name, writable_string, ACCOUNT_NAME_MAX_LENGTH);


/* switch to new account */
    
    if(current_account_node && (current_account_node != head)){
	if(current_account_node->prev)
	    current_account_node->prev->next = current_account_node->next;

	if(current_account_node->next)
	    current_account_node->next->prev = current_account_node->prev;

	current_account_node->next = head;
	current_account_node->prev = NULL;
	head->prev = current_account_node;
	head = current_account_node;
    }


    current_account_node = new_node;
    current_account = new_node->Entry;
    
/* end switch to new account */
    
    return 0;
}

al_entry* get_account(char* account_name){
    al_node* aln = get_account_node(account_name);

    if (aln) return aln->Entry;
    
    return 0;
}

al_node* get_account_node(char* account_name){
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, account_name, ACCOUNT_NAME_MAX_LENGTH))
	   return aln;
    }
    
    return 0;

}

/* appends a numeric suffix if necessary to ensure uniqueness */
void adjust_name(char* account_name){
    int i = 2;
    char other[ACCOUNT_NAME_MAX_LENGTH + 5];

    if(! strncmp(account_name, "new", 4)){
	if(new_num == 0) new_num = 1;

	if(new_num > 1){
	    i = new_num;
	}

	new_num++;

	if(new_num == 2) return;
    }
    else if(! get_account(account_name)) return;               /* name's just dandy already */

    for(; i < 100; i++){
	sprintf(other, "%s<%d>", account_name, i);
	if(! get_account(other)){
	    strncpy(account_name, other, ACCOUNT_NAME_MAX_LENGTH);
	    return;
	}
    }

    endwin();
    printf("doh!!!\n");
    exit(2);                                 /* there's 100 buffers with same name, or buffer name is too long to distinguish appended numbers */
}

void switch_to_account(char* account_name){
    al_node* aln = get_account_node(account_name);

    if(aln && (aln != current_account_node)){
	if(current_account_node){
	    if(current_account_node->prev)
		current_account_node->prev->next = current_account_node->next;
	    
	    if(current_account_node->next)
		current_account_node->next->prev = current_account_node->prev;
	    
	    current_account_node->next = head;
	    current_account_node->prev = NULL;
	    head->prev = current_account_node;
	    head = current_account_node;
	}
	    
	current_account_node = aln;
	current_account = aln->Entry;
    }

    return;
}

void kill_account(char* account_name){
    al_node* aln = get_account_node(account_name);

    if(! aln) return;

    if(aln->prev)
	aln->prev->next = aln->next;

    if(aln->next)
	aln->next->prev = aln->prev;

    if(aln == head)
	head = aln->next;

    free(aln->Entry);
    free(aln);
    
    if(aln == current_account_node){                   /* if we just killed the current account */
	if(head){                                      /* we should switch back to last account */
	    current_account_node = head;
	    current_account = head->Entry;

	    if(head->next){                            /* keep last accessed account at head    */
		al_node* tmp = head->next;

		head->prev = tmp;
		head->next = tmp->next;
		tmp->next = head;
		tmp->prev = NULL;
		head = tmp;
	    }
	}

	else{                                          /* no accounts left                      */
	    current_account_node = NULL;
	    current_account = NULL;
	    add_new_account_to_session("new");
	}
    }
}

char* get_last_account_name(){
    static char name[ACCOUNT_NAME_MAX_LENGTH];

    strncpy(name, head->Entry->account_name, ACCOUNT_NAME_MAX_LENGTH);

    return name;
}

int complete(char* name){
    int n = num_of_matches(name);

    if(n == 0) return n;
    
    if(n == 1){
	get_only_match(name);
	return n;
    }

    while(num_of_matches(name) == n){                                            /* as long as we all agree */
	if(strlen(name) == strlen(get_first_match(name))){
	    return n;
	}

	fill_next_letter_from_first_match(name);	                         /* keep adding letters */
    }

    name[strlen(name) - 1] = 0;                                                  /* we didn't all agree on last letter */

    return n;
}

int whole_namep(char* name){
    /* int i = 0; */
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, name, ACCOUNT_NAME_MAX_LENGTH))
	    return 1;
	    /* i++; */
    }

    /* if(i == 1) return 1; */

    return 0;
}

static int num_of_matches(char* name){
    int i = 0;
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, name, strlen(name)))
	    i++;
    }

    return i;
}

static void get_only_match(char* name){
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, name, strlen(name))){
	    strncpy(name, aln->Entry->account_name, ACCOUNT_NAME_MAX_LENGTH);
	    return;
	}
    }
}

static char* get_first_match(char* name){
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, name, strlen(name))){
	    return aln->Entry->account_name;
	}
    }

    return NULL;
}

static void fill_next_letter_from_first_match(char* name){
    al_node* aln;

    int n = strlen(name);

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->account_name, name, n)){
	    strncpy(name, aln->Entry->account_name, n + 1);
	    name[n + 1] = 0;
	    return;
	}
    }
}

int any_modified_accounts(){
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(aln->Entry->modified) return 1;
    }

    return 0;
}

al_entry* file_is_open(char* name){
    al_node* aln;

    for(aln = head; aln != NULL; aln = aln->next){
	if(! strncmp(aln->Entry->last_file, name, FILE_NAME_MAX_LENGTH))
	   return aln->Entry;
    }

    return NULL;
}

int write_all(){
    int i;
    al_node* aln;

    for(i = 0, aln = head; aln != NULL; aln = aln->next){
	if(aln->Entry->modified && aln->Entry->last_file[0])
	    if(! write_account_to_file(aln->Entry->Account, aln->Entry->last_file)){
		aln->Entry->modified = 0;
		i++;
	    }
    }

    return i;
}
