' Reads file LABELS-1, and optionally, LABELS-2 through LABELS-9 in the C:\LABELS\ directory
'  Files are downloaded from the cpfprint server after being ordered through DISCIS
'
' Prints one label with no command line option, or two labels with command line option '2'
'
' Copyright (c) 2004 Josiah I. Barber (GNU GPL)

  DECLARE SUB PrintLabel (CaseNum AS STRING, DefName AS STRING, DOB AS STRING)
  DECLARE SUB STXCmd (Command AS STRING)
  DECLARE SUB SOHCmd (Command AS STRING)
  DECLARE SUB WaitForPrinter ()

  FileBase$ = "C:\LABELS\LABELS-"

  FileNum = 1
  FileName$ = FileBase$ + LTRIM$(STR$(FileNum))

  CLS
  PRINT "Printing, please wait..."

  OPEN "COM1:9600,N,8,1,RS,DS0,CD0" FOR RANDOM AS #2
'  OPEN "C:\LABELS\TEST.TXT" FOR OUTPUT AS #2
 
  ' Set to imperial (non-metric) measurments
  STXCmd ("n")
 
  ' Set start of print position to 1.04 inches
  STXCmd ("O0104")

  ' Set stop position distance of 1.60 inches
  STXCmd ("f160")
  
  DIM buffer AS STRING
  DIM CaseNum AS STRING
  DIM DefName AS STRING
  DIM DOB AS STRING
  DIM c AS INTEGER
  
  ' Matches Files LABELS-1 through LABELS-9
  '
  ' Does not match LABELS-10 because LABELS-10 is LABELS~1 in DOS syntax
  '  of 8 character file names.
  '
  ' If you wanted to match more files, have users save to different base
  '  name that is less then 7 characters.  Using "LABEL-" as base would
  '  allow 99 matches instead of 9.
  '
  WHILE (LEN(DIR$(FileName$)) > 0)
    OPEN FileName$ FOR INPUT AS #1

    ' Read through file
    DO UNTIL EOF(1)
      LINE INPUT #1, buffer
    
      ' Skip Blank Lines
      WHILE ((buffer = "") AND NOT EOF(1))
        LINE INPUT #1, buffer
      WEND
    
      IF (EOF(1)) THEN
        GOTO ENDOFFILE
      END IF
   
      ' Ignore Line Feed characters
      IF (ASC(buffer) = 12) THEN
        buffer = RIGHT$(buffer, LEN(buffer) - 1)
      END IF
    
      ' Move any spaces to the end of the number for consistency with past behavior
      CaseNum = LEFT$(buffer, 9)
      CaseNum = RTRIM$(LTRIM$(CaseNum))
      CaseNum = CaseNum + SPACE$(9 - LEN(CaseNum))

      LINE INPUT #1, buffer
   
      ' Skip Blank Lines
      WHILE ((buffer = "") AND NOT EOF(1))
        LINE INPUT #1, buffer
      WEND
    
      IF (EOF(1) AND (buffer = "")) THEN
        GOTO ENDOFFILE
      END IF

      ' There is a Date of Birth included
      IF ((LEN(buffer) > 10) AND (INSTR(buffer, "/") > 0)) THEN
        DefName = LEFT$(buffer, LEN(buffer) - 11)
        DOB = RIGHT$(buffer, 10)

      ' There is not a Date of Birth included
      ELSE
        DefName = buffer
        DOB = ""
      END IF

      ' The first line of the name defined in the DISCIS 'PSR' screen is 32
      '  characters long.  If there is a second line it will be included
      '  starting at charater 33, and we don't want it.
      IF (LEN(DefName) > 32) THEN
        DefName = RTRIM$(LEFT$(DefName, 32))
      END IF

      CALL PrintLabel(CaseNum, DefName, DOB)
    LOOP

    CLOSE #1

    ' Next file
    FileNum = FileNum + 1
    FileName$ = FileBase$ + LTRIM$(STR$(FileNum))
  WEND

ENDOFFILE:
  WaitForPrinter
  CLOSE #2

  SUB PrintLabel (CaseNum AS STRING, DefName AS STRING, DOB AS STRING)
    ' Switch to label formatting mode
    STXCmd ("L")

    ' Send Case Number
    ' Rotation = 1; Font = 2; Width = 1; Height = 1; Row = 0003; Column = 0170
    PRINT #2, "121100000030170" + CaseNum + CHR$(13)

    ' Send Barcode
    ' Rotation = 1; Font = a ("Code 3 of 9" barcode, no text); wide-width = 2;
    '  thin-width = 1; Barcode-Height = 018; Row = 0003; Column = 0015
    PRINT #2, "1a2101800030015" + CaseNum + CHR$(13)

    ' Send Date of Birth
    ' Rotation = 1; Font = 1; Width = 1; Height = 1; Row = 0021; Column = 0015
    PRINT #2, "111100000210015" + DOB + CHR$(13)

    ' Send Defendant's Name
    ' Rotation = 1; Font = 9; Width = 1; Height = 1; Font-Select = A06 (6pt
    '  Triumvirate); Row = 0032; Column = 0015
    PRINT #2, "1911A0600320015" + DefName + CHR$(13)

  CMD$ = COMMAND$

  ' Accept numbers 1-3 as valid switches.  please note that if you change
  '   the range it should still be just one character long.
  IF ((LEN(CMD$) = 1) AND (VAL(CMD$) >= 1) AND (VAL(CMD$) <= 3)) THEN
    ' Set number of each label to print to number from command line
    PRINT #2, "Q000" + CMD$ + CHR$(13)
  END IF

    ' Terminate label formatting mode and print label
    PRINT #2, "E" + CHR$(13)
  END SUB

  SUB SOHCmd (Command AS STRING)
    PRINT #2, CHR$(1) + Command + CHR$(13)
  END SUB

  SUB STXCmd (Command AS STRING)
    PRINT #2, CHR$(2) + Command + CHR$(13)
  END SUB

  SUB WaitForPrinter
    DO
      ' Clear input buffer
      IF (LOC(2) > 0) THEN
        buf$ = INPUT$(LOC(2), #2)
      END IF

      ' Request Status String
      SOHCmd ("A")

      ' Wait for full Status String to show up
      WHILE (LOC(2) < 9)
      WEND

      ' Assign status string
      buf$ = INPUT$(9, #2)
    LOOP WHILE (LEFT$(buf$, 6) <> "NNNNNN")
  END SUB

