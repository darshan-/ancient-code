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

int fd;
int first_trk;
int last_trk;
struct msft start;
struct msft end;

void init();
void print_status();
void get_start_time(int track, struct msft* time);
void make_logical(struct cdrom_msf0* msf);
union cdrom_addr length(int track);
void set_abs(struct msft* addr);
void set_msf(struct msft* addr);


void c_status(){
    fd = open("/dev/cdrom", O_RDONLY | O_NONBLOCK);

    init();

    print_status();
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

