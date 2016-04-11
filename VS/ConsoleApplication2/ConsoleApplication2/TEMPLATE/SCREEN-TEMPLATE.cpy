      * SCREEN-TEMPLATE
       01 CLRSCREEN BLANK SCREEN.
       
	   01 STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
           02 LINE 1 COL 1 VALUE "A".     
       
       01 SS-STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
           02  LINE 3  COL 5  PIC 9(2) FROM WS-CURR-DAY.
           02  LINE 3  COL 7  VALUE "/".
           02  LINE 3  COL 8  PIC 9(2) FROM WS-CURR-MONTH.
           02  LINE 3  COL 10 VALUE "/".
           02  LINE 3  COL 11 PIC 9(4) FROM WS-CURR-YEAR.
           02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
           02  LINE 3  COL 50 PIC X(25) FROM WS-FUNC.
           02  LINE 6  COL 1  PIC X(80) FROM WSOULIGNE.
           02  LINE 19 COL 1  PIC X(80) FROM WSOULIGNE.
           02  LINE 20 COL 1  PIC X(60) FROM WS-MSG.
           02  LINE 21 COL 1  PIC X(60) FROM WS-INVITE.
           02  LINE 22 COL 1  VALUE ">".
           02  LINE 22 COL 9  USING WS-CHOIX.
       
       01 FOOTER.
           02 LINE 21 COL 3  PIC X(60) FROM WS-MSG.
           02 LINE 22 COL 3  PIC X(60) FROM WS-INVITE.
           02 LINE 23 COL 3  VALUE ">".
           02 LINE 23 COL 9  USING WS-CHOIX.

       01 BANNIERE.
       02 LINE 2 COL 21 VALUE "   /-------------------------------\".
       02 LINE 3 COL 21 VALUE "___|                               |___".
       02 LINE 4 COL 21 VALUE "   |                               |".
       02 LINE 5 COL 21 VALUE "   \-------------------------------/".
       
       01 INSIDE-BANNIERE.
           02 LINE 3 COL 30 "AEROCLUB".
           02 LINE 3 COL 40 FROM WS-CURR-DAY.
           02 LINE 3 COL 43 FROM WS-CURR-MONTH.
           02 LINE 3 COL 46 FROM WS-CURR-YEAR.
           02 LINE 4 COL 30 FROM WS-APP.
           02 LINE 4 COL 35 FROM WS-FUNC.

       01 AVIONG.
           02 LINE 2 COL 3 VALUE "|  __/~~\_____/~~|".
           02 LINE 3 COL 3 VALUE "+-( ======______/ ".
           02 LINE 4 COL 3 VALUE "|  ~~\|~~~        ".
           02 LINE 5 COL 3 VALUE "     ()".

       01 AVIOND.
           02 LINE 2 COL 60 VALUE "|~~\_____/~~\__  |".
           02 LINE 3 COL 60 VALUE " \______====== )-+".
           02 LINE 4 COL 60 VALUE "        ~~~|/~~  |".
           02 LINE 5 COL 60 VALUE "           ()".
        
       01 CADRE.
           02 LINE 1  COL 1 PIC X(80) FROM TIRETS.
           02 LINE 6  COL 1 PIC X(80) FROM TIRETS.
           02 LINE 20 COL 1 PIC X(80) FROM TIRETS.
           02 LINE 24 COL 1 PIC X(80) FROM TIRETS.
           02 LINE 1  COL 1 VALUE "/".
           02 LINE 2  COL 1 VALUE "|".
           02 LINE 3  COL 1 VALUE "|".
           02 LINE 4  COL 1 VALUE "|".
           02 LINE 5  COL 1 VALUE "|".
           02 LINE 6  COL 1 VALUE "|".
           02 LINE 7  COL 1 VALUE "|".
           02 LINE 8  COL 1 VALUE "|".
           02 LINE 9  COL 1 VALUE "|".
           02 LINE 10 COL 1 VALUE "|".
           02 LINE 11 COL 1 VALUE "|".
           02 LINE 12 COL 1 VALUE "|".
           02 LINE 13 COL 1 VALUE "|".
           02 LINE 14 COL 1 VALUE "|".
           02 LINE 15 COL 1 VALUE "|".
           02 LINE 16 COL 1 VALUE "|".
           02 LINE 17 COL 1 VALUE "|".
           02 LINE 18 COL 1 VALUE "|".
           02 LINE 19 COL 1 VALUE "|".
           02 LINE 20 COL 1 VALUE "|".
           02 LINE 21 COL 1 VALUE "|".
           02 LINE 22 COL 1 VALUE "|".
           02 LINE 23 COL 1 VALUE "|".
           02 LINE 24 COL 1 VALUE "\".
           02 LINE 1  COL 80 VALUE "\".
           02 LINE 2  COL 80 VALUE "|".
           02 LINE 3  COL 80 VALUE "|".
           02 LINE 4  COL 80 VALUE "|".
           02 LINE 5  COL 80 VALUE "|".
           02 LINE 6  COL 80 VALUE "|".
           02 LINE 7  COL 80 VALUE "|".
           02 LINE 8  COL 80 VALUE "|".
           02 LINE 9  COL 80 VALUE "|".
           02 LINE 10 COL 80 VALUE "|".
           02 LINE 11 COL 80 VALUE "|".
           02 LINE 12 COL 80 VALUE "|".
           02 LINE 13 COL 80 VALUE "|".
           02 LINE 14 COL 80 VALUE "|".
           02 LINE 15 COL 80 VALUE "|".
           02 LINE 16 COL 80 VALUE "|".
           02 LINE 17 COL 80 VALUE "|".
           02 LINE 18 COL 80 VALUE "|".
           02 LINE 19 COL 80 VALUE "|".
           02 LINE 20 COL 80 VALUE "|".
           02 LINE 21 COL 80 VALUE "|".
           02 LINE 22 COL 80 VALUE "|".
           02 LINE 23 COL 80 VALUE "|".
           02 LINE 24 COL 80 VALUE "/".