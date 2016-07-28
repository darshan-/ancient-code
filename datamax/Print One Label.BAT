REM Just quit if there isn't a label file to print
REM   This is important not only to save time, but to prevent backup of last files
REM   from being erased
IF NOT EXIST C:\LABELS\LABELS-1 GOTO END

REM need to echo into qbx to skip splash screen
REM ECHO A | C:\PROGXDCC\QB71\QBX.EXE /RUN C:\LABELS\PRINTLAB.BAS
C:\LABELS\PRINTLAB.EXE

REM Delete backup of last files
DEL C:\LABELS\LASTLBLS\LABELS-?

REM Backup files just printed in case they need to be reprinted for some reason
MOVE /Y C:\LABELS\LABELS-? C:\LABELS\LASTLBLS

:END