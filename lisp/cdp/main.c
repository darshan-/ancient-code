#include <sys/types.h>
#include <linux/cdrom.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

struct msft{
    int minute;
    int second;
    int frame;
    int abs_frame;
};

int flag_silent = 0;

int fd;
int first_trk;
int last_trk;
struct msft start;
struct msft end;

#ifdef CDP_LIST_PLAY
#define REQUEST_SIZE 10

char request[REQUEST_SIZE];
#endif /* CDP_LIST_PLAY */

void init();
void get_start_time(int track, struct msft*);
int play_trk(int from, int to);
int play_msf(struct msft* from, struct msft* to);
void print_status();
void print_info(int);
int drive_status();
void make_logical(struct cdrom_msf0*);
int cur_trk();
union cdrom_addr length(int);
int isnumber(char*);
int looksgood(char*);
int make_addr(int track, char*, struct cdrom_msf*);
void set_abs(struct msft*);
void set_msf(struct msft*);
void badarg(char** argv, int bad)
{
    printf("%s: junk on command line: \"%s\"\n", argv[0], argv[bad]);
    exit(1);
}

int main(int argc, char** argv)
{
    int ret;
    struct cdrom_msf addr;

    if (argv[0][strlen(argv[0]) - 1] == '1')
	fd = open("/dev/cdrom1", O_RDONLY | O_NONBLOCK);
    else
	fd = open("/dev/cdrom", O_RDONLY | O_NONBLOCK);

    if (fd == -1){
	printf("Doh! open() gave errno %d: %s\n", errno, strerror(errno));
	exit(1);
    }

    init();

    if (argc < 2){
	print_status();
	
	exit(0);
    }
    
    if (argv[1][0] == 'c'){
	if (argv[1][1] != '\0' && strcmp(argv[1], "close")){
	    badarg(argv, 1);
	}
	
	if (argc > 2){
	    badarg(argv, 2);
	}
	
	ret = drive_status();
	
	if (ret == CDROM_AUDIO_PLAY){
	    printf("the cd is playing, so the tray must already be closed.\n");
	    exit(1);
	}

	if (ret == CDROM_AUDIO_PAUSED){
	    printf("the cd is paused, so the tray must already be closed.\n");
	    exit(1);
	}

	ioctl(fd, CDROMCLOSETRAY);

	exit(0);
    }

    if (argv[1][0] == 'e'){
	if (argv[1][1] != '\0' && strcmp(argv[1], "eject")){
	    badarg(argv, 1);
	}
	
	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();
	if (ret == CDROM_AUDIO_PLAY || ret == CDROM_AUDIO_PAUSED){
	    printf("you should stop the cd first.\n");
	    exit(1);
	}

	ioctl(fd, CDROMEJECT);

	exit(0);
    }

    if (first_trk == -1){
	printf("no disc?\n");
	exit(1);
    }

    if (argv[1][0] == 'i'){
	int track = 0;
	
	if (argv[1][1] != '\0'){
	    badarg(argv, 1);
	}
	
	if (argc > 3){
	    badarg(argv, 3);
	}

	if (argc == 3){
	    if (! isnumber(argv[2])){
		badarg(argv, 2);
	    }

	    track = atoi(argv[2]);

	    if (track > last_trk || track < first_trk){
		printf("track %d is out of range.\n", track);
		exit(1);
	    }
	}
	
	print_info(track);
	print_status();
	
	exit(0);
    }
    
    if (argv[1][0] == 'p'){
	if (argv[1][1] != '\0' && strcmp(argv[1], "play") && strcmp(argv[1], "pause")){
	    badarg(argv, 1);
	}
	
	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();

	if (ret == -1){
	    printf("no disc?\n");
	    return 1;
	}

	if (ret == CDROM_AUDIO_PAUSED){
	    if (!strcmp(argv[1], "pause")){
		printf("already paused.\n");
		exit(1);
	    }

	    ioctl(fd, CDROMRESUME);
	}

	else if (ret == CDROM_AUDIO_PLAY){
	    if (!strcmp(argv[1], "play")){
		printf("already playing.\n");
		exit(1);
	    }
	    
	    ioctl(fd, CDROMPAUSE);
	}

	else{
	    if (!strcmp(argv[1], "pause")){
		printf("not playing.\n");
		exit(1);
	    }

	    play_msf(&start, &end);
	}
	print_status();
    }
    else if (looksgood(argv[1])){
	if (argc > 2){
	    badarg(argv, 2);
	}
	
	if (! make_addr(CDROM_LEADOUT, argv[1], &addr)){
	    printf("out of range.\n");
	    exit(1);
	}

	addr.cdmsf_frame0 += 2;

	addr.cdmsf_min1 = end.minute;
	addr.cdmsf_sec1 = end.second;
	addr.cdmsf_frame1 = end.frame;

	ret = drive_status();

	ioctl(fd, CDROMPLAYMSF, &addr);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}
	
	print_status();
    }
    else if ((isnumber(argv[1]) || !strcmp(argv[1], "t")) && argc > 2 && looksgood(argv[2])){
	int track;

	if (argv[1][0] == 't'){
	    if (drive_status() != CDROM_AUDIO_PAUSED &&
		drive_status() != CDROM_AUDIO_PLAY){
		printf("cd is not playing.\n");
		exit(1);
	    }
	    
	    track = cur_trk();
	}
	else{
	    track = atoi(argv[1]);
	}

	if (argc > 3){
	    badarg(argv, 3);
	}

	if (track > last_trk || track < first_trk){
	    printf("track %d is out of range.\n", track);
	    exit(1);
	}
	
	if (! make_addr(track, argv[2], &addr)){
	    printf("out of range.\n");
	    exit(1);
	}

	addr.cdmsf_frame0 += 2;

	addr.cdmsf_min1 = end.minute;
	addr.cdmsf_sec1 = end.second;
	addr.cdmsf_frame1 = end.frame;

	ret = drive_status();

	ioctl(fd, CDROMPLAYMSF, &addr);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}
	
	print_status();
    }
    else if (isnumber(argv[1])){
	int from, to;
	from = atoi(argv[1]);

	if (from < first_trk || from > last_trk){
	    printf("requested start track %d is out of range.\n", from);
	    exit(1);
	}

	if (argc > 2){
	    if (! isnumber(argv[2])){
		badarg(argv, 2);
	    }

	    if (argc > 3){
		badarg(argv, 3);
	    }

	    to = atoi(argv[2]);

	    if (to < from || to > last_trk){
		printf("requested end track %d is out of range.\n", to);
		exit(1);
	    }
	}
	else{
	    to = last_trk;
	}
	
	ret = drive_status();

	play_trk(from, to);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}

	print_status();
    }
    else if (argv[1][0] == '-' || argv[1][0] == '+'){
	int now;
	int index;
	struct cdrom_msf then;
	struct cdrom_subchnl ch;

	if (! argv[1][1] || ! isnumber(argv[1] + 1)){
	    badarg(argv, 1);
	}

	if (argc > 2){
	    badarg(argv, 2);
	}

	ch.cdsc_format = CDROM_MSF;

	if (ioctl(fd, CDROMSUBCHNL, &ch) == -1){
	    printf("erp!\n");
	    exit(1);
	}

	if (ch.cdsc_audiostatus != CDROM_AUDIO_PLAY && ch.cdsc_audiostatus != CDROM_AUDIO_PAUSED){
	    printf("cd is not playing.\n");
	    exit(1);
	}

	now = (ch.cdsc_absaddr.msf.minute*60 + ch.cdsc_absaddr.msf.second)*75 + ch.cdsc_absaddr.msf.frame;
	index = atoi(argv[1] + 1) * 75;

	if (argv[1][0] == '-'){
	    now -= index;

	    if (now < start.abs_frame){
		printf("that's before the start of the cd.\n");
		exit(1);
	    }
	}
	else{
	    now += index;

	    if (now > end.abs_frame){
		printf("that's after the end of the cd.\n");
		exit(1);
	    }
	}

	then.cdmsf_min0 = (now / 75) / 60;
	then.cdmsf_sec0 = (now / 75) % 60;
	then.cdmsf_frame0 = now % 75;
	then.cdmsf_min1 = end.minute;
	then.cdmsf_sec1 = end.second;
	then.cdmsf_frame1 = end.frame;

	ret = drive_status();

	ioctl(fd, CDROMPLAYMSF, &then);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}
	
	print_status();
    }
    else if (argv[1][0] == 'f'){
	int from;
	
	if (argv[1][1] != '\0' && strcmp(argv[1], "forward") && strcmp(argv[1], "ff")){
	    badarg(argv, 1);
	}

	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();

	if (ret == -1){
	    printf("no disc?\n");
	    return 1;
	}

	if (ret != CDROM_AUDIO_PAUSED && ret != CDROM_AUDIO_PLAY){
	    printf("cd is not playing.\n");
	    exit(1);
	}

	ret = cur_trk();

	if (ret < 1){
	    printf("i've got problems.\n");
	    exit(1);
	}

	if (ret == last_trk){
	    if (argv[1][1] == 'f'){
		from = first_trk;
	    }
	    else{
		printf("already on last track.\n");
		exit(1);
	    }
	}
	else{
	    from = ret + 1;
	}

	ret = drive_status();

	play_trk(from, last_trk);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}

	print_status();
    }
    else if (argv[1][0] == 't'){
	struct msft this_trk;
	struct cdrom_msf then;
	
	if (argv[1][1] != '\0' && strcmp(argv[1], "this")){
	    badarg(argv, 1);
	}

	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();

	if (ret == -1){
	    printf("no disc?\n");
	    return 1;
	}

	if (ret != CDROM_AUDIO_PAUSED && ret != CDROM_AUDIO_PLAY){
	    printf("cd is not playing.\n");
	    exit(1);
	}

	get_start_time(cur_trk(), &this_trk);

	then.cdmsf_min0 = this_trk.minute;
	then.cdmsf_sec0 = this_trk.second;
	then.cdmsf_frame0 = this_trk.frame;
	then.cdmsf_min1 = end.minute;
	then.cdmsf_sec1 = end.second;
	then.cdmsf_frame1 = end.frame;

	ret = drive_status();

	ioctl(fd, CDROMPLAYMSF, &then);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}
	
	print_status();
    }
    else if (argv[1][0] == 'b'){
	int from;
	
	if (argv[1][1] != '\0' && strcmp(argv[1], "backward") && strcmp(argv[1], "bb")){
	    badarg(argv, 1);
	}

	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();

	if (ret == -1){
	    printf("no disc?\n");
	    return 1;
	}

	if (ret != CDROM_AUDIO_PAUSED && ret != CDROM_AUDIO_PLAY){
	    printf("cd is not playing.\n");
	    exit(1);
	}

	ret = cur_trk();
	if (ret < first_trk){
	    printf("i've got problems.\n");
	    exit(1);
	}

	if (ret == first_trk){
	    if (argv[1][1] == 'b'){
		from = last_trk;
	    }
	    else{
		printf("already on first track.\n");
		exit(1);
	    }
	}
	else{
	    from = ret - 1;
	}

	ret = drive_status();

	play_trk(from, last_trk);

	if (ret == CDROM_AUDIO_PAUSED){
	    ioctl(fd, CDROMPAUSE);
	}
	
	print_status();
    }
    else if (argv[1][0] == 's'){
	if (argv[1][1] != '\0' && strcmp(argv[1], "stop")){
	    badarg(argv, 1);
	}
	
	if (argc > 2){
	    badarg(argv, 2);
	}

	ret = drive_status();

	if (ret == -1){
	    printf("no disc?\n");
	    return 1;
	}

	if (ret != CDROM_AUDIO_PLAY && ret != CDROM_AUDIO_PAUSED){
	    printf("cd is not playing.\n");
	    exit(1);
	}
	ioctl(fd, CDROMSTOP);

	print_status();
    }

#ifdef CDP_LIST_PLAY
    else if (argv[1][0] == 'l' && argv[1][1] == '\0'){
	union cdrom_addr len;
	int i, sec, track;

	if (argc < 3){
	    printf("empty list.\n");
	    exit(1);
	}

	for (i = 2; i < argc; i++){
	    if (! isnumber(argv[i])){
		badarg(argv, i);
	    }

	    track = atoi(argv[i]);

	    if (track > last_trk || track < first_trk){
		printf("track %d is out of range.\n", track);
		exit(1);
	    }
	}

	for (i = 2; i < argc; i++){
	    track = atoi(argv[i]);
	    len = length(track);
	    sec = len.msf.minute * 60 + len.msf.second;

	    play_trk(track, track);

	    if (i == 2){
		print_status();

		if (fork() > 0){
		    exit(0);
		}
	    }

	    if (sleep(sec) > 0){
		/* */
	    }
	}
    }
#endif /* CDP_LIST_PLAY */
    
    else {
	badarg(argv, 1);
    }

    close(fd);

    return 0;
}

void init()
{
    struct cdrom_tochdr toc_hdr;

    if (ioctl(fd, CDROMREADTOCHDR, &toc_hdr) != -1){
	first_trk = toc_hdr.cdth_trk0;
	last_trk = toc_hdr.cdth_trk1;
    }
    else{
	first_trk = -1;
	return;
    }

    get_start_time(first_trk, &start);
    get_start_time(last_trk + 1, &end);
}

void get_start_time(int track, struct msft* time)
{
    struct cdrom_tocentry te;

    if (track > last_trk) track = CDROM_LEADOUT;

    te.cdte_track = track;
    te.cdte_format = CDROM_MSF;

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	printf("this shouldn't be.\n");
	exit(2);
    }

    time->minute = te.cdte_addr.msf.minute;
    time->second = te.cdte_addr.msf.second;
    time->frame = te.cdte_addr.msf.frame;

    set_abs(time);

    if (track != CDROM_LEADOUT){
	time->abs_frame += 2;
	set_msf(time);
    }
}

void print_status()
{
    union cdrom_addr len, total;
    struct cdrom_subchnl ch;
    ch.cdsc_format = CDROM_MSF;
    
    if (ioctl(fd, CDROMSUBCHNL, &ch) == -1){
	printf("no disc?\n");
	return;
    }

    len = length(ch.cdsc_trk);
    total = length(CDROM_LEADOUT);

    make_logical(&ch.cdsc_absaddr.msf);

    switch (ch.cdsc_audiostatus){
    case CDROM_AUDIO_PLAY:
	printf("playing %d/%d %d:%02d/%d:%02d %d:%02d/%d:%02d\n",
	       ch.cdsc_trk, last_trk, ch.cdsc_reladdr.msf.minute, ch.cdsc_reladdr.msf.second, len.msf.minute, len.msf.second,
	       ch.cdsc_absaddr.msf.minute, ch.cdsc_absaddr.msf.second, total.msf.minute, total.msf.second);
	break;
    case CDROM_AUDIO_PAUSED:
	printf("paused %d/%d %d:%02d/%d:%02d %d:%02d/%d:%02d\n",
	       ch.cdsc_trk, last_trk, ch.cdsc_reladdr.msf.minute, ch.cdsc_reladdr.msf.second, len.msf.minute, len.msf.second,
	       ch.cdsc_absaddr.msf.minute, ch.cdsc_absaddr.msf.second, total.msf.minute, total.msf.second);
        break;
    case CDROM_AUDIO_NO_STATUS:
	printf("stopped %d %d:%02d\n", last_trk, total.msf.minute, total.msf.second);
        break;
    case CDROM_AUDIO_INVALID:
	printf("audio status not supported\n");
        break;
    case CDROM_AUDIO_COMPLETED:
	printf("stopped %d %d:%02d\n", last_trk, total.msf.minute, total.msf.second);
        break;
    case CDROM_AUDIO_ERROR:
	printf("audio play stopped due to error\n");
        break;
    default:
	printf("duh, i dunno.\n");
	break;
    }
}

void print_info(int track)
{
    union cdrom_addr len;
    int i;

    if (first_trk == -1){
	printf("no disc?\n");
	return;
    }

    if (track >= first_trk && track <= last_trk){
	len = length(track);

	printf("%d - %d:%02d\n", track, len.msf.minute, len.msf.second);
    }

    else{
	for (i = first_trk; i <= last_trk; i++){
	    len = length(i);

	    printf("%d - %d:%02d\n", i, len.msf.minute, len.msf.second);
	}
    }
}

int drive_status()
{
    struct cdrom_subchnl ch;

    ch.cdsc_format = CDROM_MSF;
    
    if (ioctl(fd, CDROMSUBCHNL, &ch) == -1){
	return -1;
    }

    return ch.cdsc_audiostatus;
}

int cur_trk()
{
    struct cdrom_subchnl ch;

    ch.cdsc_format = CDROM_MSF;
    
    if (ioctl(fd, CDROMSUBCHNL, &ch) == -1){
	return -1;
    }

    return ch.cdsc_trk;
}

int isnumber(char* str)
{
    while (*str){
	if (*str < '0' || *str > '9')
	    return 0;
	str++;
    }
    return 1;
}

int looksgood(char* str)
{
    int colon = 0;
    int left = 0;
    int right = 0;

    while (*str){
	if (*str < '0' || *str > '9'){
	    if (*str != ':' || colon)
		return 0;
	    colon = 1;
	}
	else if (colon) right++;
	else left++;

	str++;
    }

    if (right == 2 && (left == 1 || left == 2))
	return 1;
    
    return 0;
}

// TODO: do i need this?
void add_addr(int track, struct cdrom_msf* msf)
{
    int i, j;
    struct cdrom_tocentry te;

    te.cdte_track = track;
    te.cdte_format = CDROM_MSF;

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	exit(2);
    }

    i = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
    j = (msf->cdmsf_min0*60 + msf->cdmsf_sec0)*75 + msf->cdmsf_frame0;

    j += i;

    msf->cdmsf_min0 = (j / 75) / 60;
    msf->cdmsf_sec0 = (j / 75) % 60;
    msf->cdmsf_frame0  = j % 75;
}

/* makes cdrom_msf out of str.
   adds address of track.  if track is CDROM_LEADOUT,
   adds address of beginning of cd.
*/
int make_addr(int track, char* str, struct cdrom_msf* addr)
{
    struct cdrom_tocentry te;
    int start, end, mid;

    if (! looksgood(str))
	return 0;

    addr->cdmsf_min0 = atoi(str);
    while (*str != ':') *str++;
    addr->cdmsf_sec0 = atoi(str + 1);
    addr->cdmsf_frame0 = 0;

    if (track == CDROM_LEADOUT)
	add_addr(first_trk, addr);
    else
	add_addr(track, addr);

    mid = (addr->cdmsf_min0*60 + addr->cdmsf_sec0)*75 + addr->cdmsf_frame0;

    te.cdte_track = track;
    te.cdte_format = CDROM_MSF;

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	exit(2);
    }

    if (te.cdte_track == CDROM_LEADOUT){
	end = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
	te.cdte_track = first_trk;
    }
    else{
	start = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
	te.cdte_track++;
	if (te.cdte_track > last_trk) te.cdte_track = CDROM_LEADOUT;
    }

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	exit(2);
    }

    if (te.cdte_track == first_trk)
	start = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
    else
	end = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;

    if (mid < start)
	return 0;

    if (mid > end)
	return 0;

    return 1;
}

/* subtracts address of the begining of the cd,
   so it lookes like the cd starts at 0:00
*/
void make_logical(struct cdrom_msf0* msf)
{
    int j = (msf->minute*60 + msf->second)*75 + msf->frame - start.abs_frame;

    msf->minute = (j / 75) / 60;
    msf->second = (j / 75) % 60;
    msf->frame  = j % 75;
}

union cdrom_addr length(int track)
{
    int start, end;
    union cdrom_addr len;
    struct cdrom_tocentry te;

    te.cdte_track = track;
    te.cdte_format = CDROM_MSF;

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	printf("errno %d: %s\n", errno, strerror(errno));
	exit(2);
    }

    if (te.cdte_track == CDROM_LEADOUT){
	end = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
	te.cdte_track = first_trk;
    }
    else{
	start = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
	te.cdte_track++;
	if (te.cdte_track > last_trk) te.cdte_track = CDROM_LEADOUT;
    }

    if (ioctl(fd, CDROMREADTOCENTRY, &te) == -1){
	printf("errno %d: %s\n", errno, strerror(errno));
	exit(2);
    }

    if (te.cdte_track == first_trk)
	start = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;
    else
	end = (te.cdte_addr.msf.minute*60 + te.cdte_addr.msf.second)*75 + te.cdte_addr.msf.frame;

    len.msf.minute = ((end - start) / 75) / 60;
    len.msf.second = ((end - start) / 75) % 60;

    return len;
}

void set_abs(struct msft* addr)
{
    addr->abs_frame = (addr->minute*60 + addr->second)*75 + addr->frame;
}

void set_msf(struct msft* addr)
{
    addr->minute = addr->abs_frame / 75 / 60;
    addr->second = addr->abs_frame / 75 % 60;
    addr->frame = addr->abs_frame % 75;
}

int play_trk(int from, int to)
{
    struct msft f;
    struct msft t;

    get_start_time(from, &f);
    get_start_time(to + 1, &t);
    return play_msf(&f, &t);
}

int play_msf(struct msft* from, struct msft* to)
{
    struct cdrom_msf addr;
    
    addr.cdmsf_min0 = from->minute;
    addr.cdmsf_sec0 = from->second;
    addr.cdmsf_frame0 = from->frame;
    addr.cdmsf_min1 = to->minute;
    addr.cdmsf_sec1 = to->second;
    addr.cdmsf_frame1 = to->frame;

    return ioctl(fd, CDROMPLAYMSF, &addr);
}
