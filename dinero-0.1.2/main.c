/* main.c */

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

#include <stdio.h>
#include <string.h>

#include "dinero.h"


int main(int argc, char* argv[]){
    if(0 /* if options are no good */){
	argv[0] = extract_last_field(argv[0]);
	printf("Usage: %s [file]\n", argv[0]);

	return 1;
    }

    if(argc > 1) start_screen(argc - 1, argv + 1);
    else start_screen(0, NULL);
     
    process_input();

    end_screen();

    return 0;
}
