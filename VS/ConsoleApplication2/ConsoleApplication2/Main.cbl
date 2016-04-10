        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN.
        AUTHOR. HUGO SARACINO.

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            INPUT-OUTPUT SECTION.

        DATA DIVISION.
            FILE SECTION.

            WORKING-STORAGE SECTION.
            01  WS_CURR_DATE_FIELDS.
                05  WS_CURR_DATE.
                    10  WS_CURR_YEAR    PIC  9(4).
                    10  WS_CURR_MONTH   PIC  9(2).
                    10  WS_CURR_DAY     PIC  9(2).
                05  WS_CURR_TIME.
                    10  WS_CURR_HOUR    PIC  9(2).
                    10  WS_CURR_MINUTE  PIC  9(2).
                    10  WS_CURR_SECOND  PIC  9(2).
                    10  WS_CURR_MS      PIC  9(2).
                05  WS_DIFF_FROM_GMT    PIC S9(4).
            77  WS_FUNC     PIC X(25)   VALUE "MENU PRINCIPAL".
            77  WS_LINE     PIC X(80)   VALUE "______________________".
            77  WS_MTITLE   PIC X(80)   VALUE "Veuillez choiivantes : ".
            77  WS_MENUF0   PIC X(80)   VALUE "    0 - Lista revisers.".
            77  WS_MENUF1   PIC X(80)   VALUE "    1 - Depot de vol.".
            77  WS_MENUF2   PIC X(80)   VALUE "    2 - Enregis'un vol.".
            77  WS_MENUF3   PIC X(80)   VALUE "    3 - Mise-a-pilotes.".
            77  WS_MENUF4   PIC X(80)   VALUE "    4 - Mise-a-vions.".
            77  WS_MENUF5   PIC X(80)   VALUE "    5 - Recapit avions.".
            77  WS_MENUF6   PIC X(80)   VALUE "    6 - Recapi pilotes.".
            77  WS_MENUF7   PIC X(80)   VALUE "    7 - Facturation de ".                
            77  WS_MQUIT    PIC X(80)   VALUE "    Q - Quitter l'appl ".     
            77  WS_MSG      PIC X(50)   VALUE SPACES.
            77  WS_INVITE   PIC X(50)   VALUE SPACES.
            77  WS_CHOICE   PIC X       VALUE SPACES.
            77  WS_CONTINUE PIC 9.

            SCREEN SECTION.
            01  CLRSCREEN BLANK SCREEN.
            01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                02  LINE 3  COL 1  PIC 9(2) FROM WS_CURR_DAY.
                02  LINE 3  COL 3  VALUE "/".
                02  LINE 3  COL 4  PIC 9(2) FROM WS_CURR_MONTH.
                02  LINE 3  COL 6  VALUE "/".
                02  LINE 3  COL 7  PIC 9(4) FROM WS_CURR_YEAR.
                02  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
                02  LINE 3  COL 66 PIC X(25) FROM WS_FUNC.
                02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 8  COL 1 PIC X(80) FROM WS_MTITLE.
                02  LINE 9  COL 1 PIC X(80) FROM WS_MENUF0.
                02  LINE 10 COL 1 PIC X(80) FROM WS_MENUF1.
                02  LINE 11 COL 1 PIC X(80) FROM WS_MENUF2.
                02  LINE 12 COL 1 PIC X(80) FROM WS_MENUF3.
                02  LINE 13 COL 1 PIC X(80) FROM WS_MENUF4.
                02  LINE 14 COL 1 PIC X(80) FROM WS_MENUF5.
                02  LINE 15 COL 1 PIC X(80) FROM WS_MENUF6.
                02  LINE 16 COL 1 PIC X(80) FROM WS_MENUF7.
                02  LINE 17 COL 1 PIC X(80) FROM WS_MQUIT.
                02  LINE 18 COL 1 VALUE "CHOIX : ".
                02  LINE 18 COL 10 PIC X USING WS_CHOICE.
                02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 20 COL 1 PIC X(50) FROM WS_MSG.

        PROCEDURE DIVISION.
        PRINCIPAL SECTION.
            MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.
            MOVE 1 TO WS_CONTINUE.

            PERFORM UNTIL WS_CONTINUE = 0
                DISPLAY CLRSCREEN
                DISPLAY STDSCREEN
                ACCEPT STDSCREEN

                EVALUATE WS_CHOICE
                    WHEN "0"
                        MOVE SPACES TO WS_CHOICE
                        CALL "F0" USING WS_CONTINUE
                    WHEN "1"
                        MOVE SPACES TO WS_CHOICE
                        CALL "F1" USING WS_CONTINUE
                    WHEN "2" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F2" USING WS_CONTINUE
                    WHEN "3" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F3" USING WS_CONTINUE
                    WHEN "4" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F4" USING WS_CONTINUE
                    WHEN "5" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F5" USING WS_CONTINUE
                    WHEN "6" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F6" USING WS_CONTINUE
                    WHEN "7" 
                        MOVE SPACES TO WS_CHOICE
                        CALL "F7" USING WS_CONTINUE
                    WHEN "Q" 
                        MOVE SPACES TO WS_CHOICE
                        MOVE 0 TO WS_CONTINUE
                    WHEN OTHER 
                        MOVE SPACES TO WS_CHOICE
                        MOVE "Option invalide." TO WS_MSG
                END-EVALUATE

            END-PERFORM.

            STOP RUN.