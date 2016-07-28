void move(int c, int m, int b)
{
    if ((m != 0) && (c > m)){
	printf("Munch, munch.\n");
	return;
    }

    if ((m != 3) && (m > c)){
	printf("Munch, munch.\n");
	return;
    }

    if ((c == 0) && (m == 0)){
	printf("I won!\n");
	return;
    }
    
    if (b == 1){
	if (c >= 2){
	    move(c-2, m, 0);
	}
	
	if (m >= 2){
	    move(c, m-2, 0);
	}

	if ((c > 0) && (m > 0)){
	    move(c-1, m-1, 0);
	}

	if (c > 0){
	    move(c-1, m, 0);
	}

	if (m > 0){
	    move(c, m-1, 0);
	}
    }
    else{  /* (b == 0)*/
	if (c <= 1){
	    move(c+2, m, 1);
	}

	if (m <= 1){
	    move(c, m+2, 1);
	}

	if ((c < 3) && (m < 3)){
	    move(c+1, m+1, 1);
	}

	if (c < 3){
	    move(c+1, m, 1);
	}

	if (m < 3){
	    move(c, m+1, 1);
	}
    }
}
