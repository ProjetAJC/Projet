        IDENTIFICATION DIVISION.
        PROGRAM-ID. F0.
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
            77  WS_FUNC         PIC X(25).
            77  WS_LINE         PIC X(80).
            77  WS_DESC1        PIC X(80).
            77  WS_DESC2        PIC X(80).
            77  WS_DESC3        PIC X(80).
            77  WS_RECAP        PIC X(80).
            77  WS_NVOL         PIC X(80).
            77  WS_HDEP         PIC X(80).
            77  WS_HARR         PIC X(80).
            77  WS_NAVION       PIC X(80).
            77  WS_CIVPILOTE    PIC X(80).
            77  WS_WARNING      PIC X(80).
            77  WS_OPTIONS      PIC X(80).
            77  WS_OPTBACK      PIC X(80).
            77  WS_OPTQUIT      PIC X(80).
            77  WS_MSG          PIC X(50).
            77  WS_INVITE       PIC X(50).
            77  WS_NAV          PIC X.
            77  WS_QUERY        PIC 9(3).
            77  WS_VALID        PIC X.
            77  WS_QUIT         PIC 9.
    
            LINKAGE SECTION.
            77  LS_QUIT         PIC 9.
    
            SCREEN SECTION.
            01  CLRSCREEN BLANK SCREEN.

            01  STDSCREEN.
                05  STDHEADER BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                    10  LINE 3  COL 2 PIC 9(2) FROM WS_CURR_DAY.
                    10  LINE 3  COL 4 VALUE "/".
                    10  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_MONTH.
                    10  LINE 3  COL 7 VALUE "/".
                    10  LINE 3  COL 8 PIC 9(4) FROM WS_CURR_YEAR.
                    10  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
                    10  LINE 3  COL 62 PIC X(25) FROM WS_FUNC.
                    10  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
                05  STDBODY BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
                    10  LINE 8  COL 2 PIC X(80) FROM WS_DESC1.
                    10  LINE 9  COL 2 PIC X(80) FROM WS_DESC2.
                    10  LINE 10 COL 2 PIC X(80) FROM WS_DESC3.
                    10  LINE 15 COL 2 PIC X(80) FROM WS_OPTIONS.
                    10  LINE 16 COL 2 PIC X(80) FROM WS_OPTBACK.
                    10  LINE 17 COL 2 PIC X(80) FROM WS_OPTQUIT.
                    10  LINE 18 COL 2 PIC X USING WS_NAV.
                05  STDFOOTER BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                    10  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
                    10  LINE 20 COL 2 PIC X(50) FROM WS_MSG.
                    10  LINE 21 COL 2 PIC X(50) FROM WS_INVITE.
                    10  LINE 22 COL 2 PIC 9(3) USING WS_QUERY.

            01  VALSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                02  LINE 3  COL 1 PIC 9(2) FROM WS_CURR_DAY.
                02  LINE 3  COL 3 VALUE "/".
                02  LINE 3  COL 4 PIC 9(2) FROM WS_CURR_MONTH.
                02  LINE 3  COL 6 VALUE "/".
                02  LINE 3  COL 7 PIC 9(4) FROM WS_CURR_YEAR.
                02  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
                02  LINE 3  COL 61 PIC X(25) FROM WS_FUNC.
                02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 8  COL 1 PIC X(80) FROM WS_RECAP.
                02  LINE 10 COL 1 PIC X(80) FROM WS_NVOL.
                02  LINE 11 COL 1 PIC X(80) FROM WS_HDEP.
                02  LINE 12 COL 1 PIC X(80) FROM WS_HARR.
                02  LINE 13 COL 1 PIC X(80) FROM WS_NAVION.
                02  LINE 13 COL 22 PIC 9(3) FROM WS_QUERY.
                02  LINE 14 COL 1 PIC X(80) FROM WS_CIVPILOTE.
                02  LINE 16 COL 1 PIC X(80) FROM WS_WARNING.
                02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 20 COL 1 PIC X(50) FROM WS_MSG.
                02  LINE 21 COL 1 PIC X(50) FROM WS_INVITE.
                02  LINE 22 COL 1 PIC X USING WS_VALID.

        PROCEDURE DIVISION USING LS_QUIT.
        PRINCIPAL SECTION.
            PERFORM INIT_VALUES_F0.

            PERFORM UNTIL WS_QUIT = 0
                PERFORM REFRESH_SCREEN_F0
                IF WS_QUERY <> 0 THEN
                    PERFORM SQL_TREATMENT_F0
                ELSE
                    PERFORM NAV_TREATMENT_F0
                END-IF
                MOVE ZEROES TO WS_QUERY
            END-PERFORM.

            EXIT PROGRAM.

        INIT_VALUES_F0.
            MOVE "REVISIONS/CONTROLES" TO WS_FUNC.
            MOVE ALL "-" TO WS_LINE.
            MOVE "Page d'enregistrement des vols." TO WS_DESC1.
            MOVE "    1 - Saisissez le numero de l'avion rentrant."
               TO WS_DESC2.                                          
            MOVE "    2 - Validez les informations." TO WS_DESC3.
            MOVE "RECAPITULATIF" TO WS_RECAP.
            MOVE "Numero de vol : " TO WS_NVOL.
            MOVE "Heure de depart : " TO WS_HDEP.
            MOVE "Heure d'arrivee : " TO WS_HARR.
            MOVE "Numero de l'avion : " TO WS_NAVION.
            MOVE "Pilote : " TO WS_CIVPILOTE.
            STRING "La validation de ces informations entrainera"
                   " la mise a jour de la BDD" INTO WS_WARNING.         
            MOVE "Options de navigation :" TO WS_OPTIONS.
            MOVE "    M - Retourner au menu principal." TO WS_OPTBACK.
            MOVE "    Q - Quitter l'application." TO WS_OPTQUIT.
            MOVE SPACES TO WS_MSG.
            MOVE "Veuillez saisir l'identifiant de l'avion."
               TO WS_INVITE.                                            
            MOVE SPACES TO WS_NAV.
            MOVE ZEROES TO WS_QUERY.
            MOVE SPACES TO WS_VALID.
            MOVE 1 TO WS_QUIT.
            MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.

        REFRESH_SCREEN_F0.
            DISPLAY CLRSCREEN.
            DISPLAY STDSCREEN.
            ACCEPT STDSCREEN.

        NAV_TREATMENT_F0.
            EVALUATE WS_NAV
                WHEN SPACES
                    MOVE SPACES TO WS_MSG
                WHEN "M"
                    MOVE ZEROES TO WS_QUIT
                WHEN "Q"
                    MOVE ZEROES TO WS_QUIT, LS_QUIT
                WHEN OTHER
                    MOVE "Option invalide." TO WS_MSG
            END-EVALUATE.
            MOVE SPACES TO WS_NAV.

        SQL_TREATMENT_F0.
            PERFORM SQL_QUERY_F0.
            PERFORM SQL_RECAP_F0.
            EVALUATE WS_VALID
                WHEN "O"
                    PERFORM SQL_UPDATE_F0
                WHEN "N"
                    PERFORM INIT_VALUES_F0
                    DISPLAY CLRSCREEN
                    DISPLAY STDSCREEN
            END-EVALUATE.
            MOVE SPACES TO WS_VALID.

        SQL_QUERY_F0.
    

        SQL_RECAP_F0.
            MOVE "Validez vous ces informations ? O/N" TO WS_INVITE.
            DISPLAY CLRSCREEN.
            DISPLAY VALSCREEN.
            ACCEPT WS_VALID.
            PERFORM UNTIL WS_VALID = "O" OR WS_VALID = "N"
                MOVE "Option invalide." TO WS_MSG
                MOVE SPACES TO WS_VALID
                DISPLAY CLRSCREEN
                DISPLAY VALSCREEN
                ACCEPT WS_VALID
            END-PERFORM.

        SQL_UPDATE_F0.
            PERFORM INIT_VALUES_F0.
            DISPLAY CLRSCREEN.
            DISPLAY STDSCREEN.




