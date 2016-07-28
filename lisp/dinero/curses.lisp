;;;; curses.lisp - interface to curses library

(load-1-foreign "/lib/libncurses.so.5.2")

(def-alien-routine initscr void)
;int keypad(WINDOW *win, bool bf);
(def-alien-routine keypad void (win int) (bf char))
;(def-alien-variable ("stdscr" stdscr) int)

(def-alien-routine cbreak void)
(def-alien-routine noecho void)
(def-alien-routine raw void)
(def-alien-routine refresh void)
(def-alien-routine endwin void)
(def-alien-routine move void (y int) (x int))
(def-alien-routine ("addch" curses-addch) void (ch char))

(defun addch (c)
  (curses-addch (char-code c)))

(def-alien-routine ("getch" curses-getch) int)

(defun getch ()
  (let ((c (curses-getch)))
	(if (or (< c 0)
			(> c 256))
		c
		(code-char c))))

(def-alien-routine attrset void (attrs int))

;;; attributes
(defconstant A_NORMAL     #x00000000)
(defconstant A_STANDOUT   #x00010000)
(defconstant A_UNDERLINE  #x00020000)
(defconstant A_REVERSE    #x00040000)
(defconstant A_BLINK      #x00080000)
(defconstant A_DIM        #x00100000)
(defconstant A_BOLD       #x00200000)
(defconstant A_ALTCHARSET #x00400000)
(defconstant A_INVIS      #x00800000)

(defconstant A_PROTECT    #x01000000)
(defconstant A_HORIZONTAL #x02000000)
(defconstant A_LEFT       #x04000000)
(defconstant A_LOW        #x08000000)
(defconstant A_RIGHT      #x10000000)
(defconstant A_TOP        #x20000000)
(defconstant A_VERTICAL   #x40000000)

;;; keys
(defconstant KEY_MIN        #o401)
(defconstant KEY_DOWN       #o402)
(defconstant KEY_UP         #o403)
(defconstant KEY_LEFT       #o404)
(defconstant KEY_RIGHT      #o405)
(defconstant KEY_HOME       #o406)
;(defconstant KEY_BEG        #o542)
(defconstant KEY_BACKSPACE  #o407)
(defconstant KEY_NPAGE      #o522)
(defconstant KEY_PPAGE      #o523)
(defconstant KEY_END        #o550)
;(defconstant KEY_LL         #o533)
