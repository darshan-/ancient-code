;(load-1-foreign "/lib/libc.so.6")

(def-alien-routine ioctl INT (fd INT) (request INT))
(def-alien-routine ("open" unix-open) int (path c-string) (flags int))
(def-alien-routine ("close" unix-close) int (fd int))
(def-alien-routine strerror c-string (errnum int))


;;;; linux ioctl commands, from /usr/include/linux/cdrom.h
(defconstant CDROMPAUSE              #x5301) ;Pause Audio Operation 
(defconstant CDROMRESUME             #x5302) ;Resume paused Audio Operation
(defconstant CDROMPLAYMSF            #x5303) ;Play Audio MSF (struct cdrom_msf)
(defconstant CDROMPLAYTRKIND         #x5304) ;Play Audio Track/index (struct cdrom_ti)
(defconstant CDROMREADTOCHDR         #x5305) ;Read TOC header (struct cdrom_tochdr)
(defconstant CDROMREADTOCENTRY       #x5306) ;Read TOC entry (struct cdrom_tocentry)
(defconstant CDROMSTOP               #x5307) ;Stop the cdrom drive
(defconstant CDROMSTART              #x5308) ;Start the cdrom drive
(defconstant CDROMEJECT              #x5309) ;Ejects the cdrom media
(defconstant CDROMVOLCTRL            #x530a) ;Control output volume (struct cdrom_volctrl)
(defconstant CDROMSUBCHNL            #x530b) ;Read subchannel data (struct cdrom_subchnl)
(defconstant CDROMREADMODE2          #x530c) ;Read CDROM mode 2 data (2336 Bytes) (struct cdrom_read)
(defconstant CDROMREADMODE1          #x530d) ;Read CDROM mode 1 data (2048 Bytes) (struct cdrom_read)
(defconstant CDROMREADAUDIO          #x530e) ;(struct cdrom_read_audio)
(defconstant CDROMEJECT_SW           #x530f) ;enable(1)/disable(0) auto-ejecting
(defconstant CDROMMULTISESSION       #x5310) ;Obtain the start-of-last-session address of multi session disks (struct cdrom_multisession)
(defconstant CDROM_GET_MCN           #x5311) ;Obtain the "Universal Product Code" if available (struct cdrom_mcn)
(defconstant CDROMRESET              #x5312) ;hard-reset the drive
(defconstant CDROMVOLREAD            #x5313) ;Get the drive's volume setting (struct cdrom_volctrl)
(defconstant CDROMREADRAW            #x5314) ;read data in raw mode (2352 Bytes) (struct cdrom_read)

(defconstant CDROMCLOSETRAY          #x5319) ;pendant of CDROMEJECT
(defconstant CDROM_SET_OPTIONS       #x5320) ;Set behavior options
(defconstant CDROM_CLEAR_OPTIONS     #x5321) ;Clear behavior options
(defconstant CDROM_SELECT_SPEED      #x5322) ;Set the CD-ROM speed
(defconstant CDROM_SELECT_DISC       #x5323) ;Select disc (for juke-boxes)

(defconstant CDROM_MEDIA_CHANGED     #x5325) ;Check is media changed 
(defconstant CDROM_DRIVE_STATUS      #x5326) ;Get tray position, etc.
(defconstant CDROM_DISC_STATUS       #x5327) ;Get disc type, etc.
(defconstant CDROM_CHANGER_NSLOTS    #x5328) ;Get number of slots
(defconstant CDROM_LOCKDOOR          #x5329) ;lock or unlock door
(defconstant CDROM_DEBUG             #x5330) ;Turn debug messages on/off
(defconstant CDROM_GET_CAPABILITY    #x5331) ;get capabilities


(defvar fd (unix-open "/dev/cdrom" #o4000)) ; should be a constant, just a var for now so i can play with it if necessary

(defun cdrom-do (command)
  (ioctl fd command))

(defun eject ()
  (let ((ret (cdrom-do cdrom_drive_status)))
	(cond ((= ret 4) (cdrom-do cdromeject))
		  ((= ret 1) (cdrom-do cdromclosetray))
		  (t (format t "i don't know... ~A~%" ret)))))


