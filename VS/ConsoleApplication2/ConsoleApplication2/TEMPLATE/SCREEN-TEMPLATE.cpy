      * SCREEN-TEMPLATE 
       01  SS-STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
            02  LINE 3  COL 5 PIC 9(2) FROM WS-CURR-DAY.
            02  LINE 3  COL 7 VALUE "/".
            02  LINE 3  COL 8 PIC 9(2) FROM WS-CURR-MONTH.
            02  LINE 3  COL 10 VALUE "/".
            02  LINE 3  COL 11 PIC 9(4) FROM WS-CURR-YEAR.
            02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
            02  LINE 3  COL 50 PIC X(25) FROM WS-FUNC.
            02  LINE 6  COL 1 PIC X(80) FROM WSOULIGNE.
            02  LINE 19  COL 1 PIC X(80) FROM WSOULIGNE.
            02  LINE 20 COL 1 PIC X(60) FROM WS-MSG.
            02  LINE 21 COL 1 PIC X(60) FROM WS-INVITE.
            02  LINE 22 COL 1 VALUE "Choix :".
            02  LINE 22 COL 9 USING WS-CHOIX.