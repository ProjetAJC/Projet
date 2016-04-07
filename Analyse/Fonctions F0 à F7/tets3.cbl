IDENTIFICATION DIVISION.
PROGRAM-ID. F2.

DATA DIVISION.
   WORKING-STORAGE SECTION.
   77 a PIC 9.

   01 WS-STUDENT-ID PIC 9(4) VALUE 1000.
   01 WS-STUDENT-NAME PIC A(15) VALUE 'Tim'.
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
    01 B.
      02 PIC X(6) FROM "line 1" LINE 9 COL 1.
      02 PIC X(6) FROM "line 2" LINE 10 COL 1.
      02 PIC X(6) FROM "line 3" LINE 11 COL 1.
    01 C.
      02 PIC X(6) FROM "line 4" LINE 9 COL 1.
      02 PIC X(6) FROM "line 5" LINE 10 COL 1.
      02 PIC X(6) FROM "line 6" LINE 11 COL 1.
		
	01  CLRSCREEN BLANK SCREEN.
	
    01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
        02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
        02  LINE 8  COL 1 PIC X(80) FROM WS_DESC1.
        02  LINE 9  COL 1 PIC X(80) FROM WS_DESC2.
        02  LINE 10 COL 1 PIC X(80) FROM WS_DESC3.
        02  LINE 15 COL 1 PIC X(80) FROM WS_OPTIONS.
        02  LINE 16 COL 1 PIC X(80) FROM WS_OPTBACK.
        02  LINE 17 COL 1 PIC X(80) FROM WS_OPTQUIT.
        02  LINE 18 COL 1 PIC X USING WS_NAV.

    01  VALSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
        02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
        02  LINE 8  COL 1 PIC X(80) FROM WS_RECAP.
        02  LINE 10 COL 1 PIC X(80) FROM WS_NVOL.
        02  LINE 11 COL 1 PIC X(80) FROM WS_HDEP.
        02  LINE 12 COL 1 PIC X(80) FROM WS_HARR.
        02  LINE 13 COL 1 PIC X(80) FROM WS_NAVION.
        02  LINE 13 COL 22 PIC 9(3) FROM WS_QUERY.
        02  LINE 14 COL 1 PIC X(80) FROM WS_CIVPILOTE.
        02  LINE 16 COL 1 PIC X(80) FROM WS_WARNING.
		
	  
	  
PROCEDURE DIVISION.
PRINCIPAL SECTION.
    CALL 'UTIL' USING WS_QUIT.

    PERFORM INIT_VALUES.

    PERFORM UNTIL WS_QUIT = 0
        PERFORM REFRESH_SCREEN
        IF WS_QUERY <> 0 THEN
            PERFORM SQL_TREATMENT
        ELSE
            PERFORM NAV_TREATMENT
        END-IF
        MOVE ZEROES TO WS_QUERY
    END-PERFORM.

    EXIT PROGRAM.

INIT_VALUES.
    MOVE "ENREGISTREMENT VOL" TO WS_FUNC.
    MOVE "--------------------------------------------------------------------------------" TO WS_LINE.
    MOVE "Page d'enregistrement des vols." TO WS_DESC1.
    MOVE "    1 - Saisissez le numero de l'avion rentrant." TO WS_DESC2.
    MOVE "    2 - Validez les informations." TO WS_DESC3.
    MOVE "RECAPITULATIF" TO WS_RECAP.
    MOVE "Numero de vol : " TO WS_NVOL.
    MOVE "Heure de depart : " TO WS_HDEP.
    MOVE "Heure d'arrivee : " TO WS_HARR.
    MOVE "Numero de l'avion : " TO WS_NAVION.
    MOVE "Pilote : " TO WS_CIVPILOTE.
    MOVE "La validation de ces informations entrainera la mise a jour de la BDD" TO WS_WARNING.
    MOVE "Options de navigation :" TO WS_OPTIONS.
    MOVE "    M - Retourner au menu principal." TO WS_OPTBACK.
    MOVE "    Q - Quitter l'application." TO WS_OPTQUIT.
    MOVE SPACES TO WS_MSG.
    MOVE "Veuillez saisir l'identifiant de l'avion." TO WS_INVITE.
    MOVE SPACES TO WS_NAV.
    MOVE ZEROES TO WS_QUERY.
    MOVE SPACES TO WS_VALID.
    MOVE 1 TO WS_QUIT.

REFRESH_SCREEN.
    DISPLAY STDSCREEN.
    ACCEPT STDSCREEN.

NAV_TREATMENT.
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

SQL_TREATMENT.
    PERFORM SQL_QUERY.
    PERFORM SQL_RECAP.
    EVALUATE WS_VALID
        WHEN "O"
            PERFORM SQL_UPDATE
        WHEN "N"
            PERFORM INIT_VALUES
            DISPLAY CLRSCREEN
            DISPLAY STDSCREEN
    END-EVALUATE.
    MOVE SPACES TO WS_VALID.

SQL_QUERY.
    

SQL_RECAP.
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

SQL_UPDATE.
    PERFORM INIT_VALUES.
    DISPLAY CLRSCREEN.
    DISPLAY STDSCREEN.
END PROGRAM F2.




IDENTIFICATION DIVISION.
PROGRAM-ID. UTIL.

DATA DIVISION.
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
    77 A PIC 9.
    77  WS_FUNC     PIC X(25)   VALUE "LISTE AVIONS/PILOTES".
    77  WS_LINE     PIC X(80)   VALUE "________________________________________________________________________________".
    77  WS_MTITLE   PIC X(80)   VALUE "Veuillez choisir parmis les options suivantes : ".
    77  WS_MBACK    PIC X(80)   VALUE "    M - Retourner au menu principal.".
    77  WS_MQUIT    PIC X(80)   VALUE "    Q - Quitter l'application.".
    77  WS_MSG      PIC X(50)   VALUE SPACES.
    77  WS_INVITE   PIC X(50)   VALUE SPACES.
    77  WS_CHOICE   PIC X       VALUE SPACES.
    77  WS_CONTINUE PIC 9.
   LINKAGE SECTION.
   01 LS_QUIT PIC 9.

  SCREEN SECTION.
    01 SC.
        02 PIC 9(4) FROM WS_CURR_YEAR LINE 5 COL 5.
    01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
        02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_DAY.
        02  LINE 3  COL 7 VALUE "/".
        02  LINE 3  COL 8 PIC 9(2) FROM WS_CURR_MONTH.
        02  LINE 3  COL 10 VALUE "/".
        02  LINE 3  COL 11 PIC 9(4) FROM WS_CURR_YEAR.
        02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
        02  LINE 3  COL 50 PIC X(25) FROM WS_FUNC.
        02  LINE 20 COL 1 PIC X(50) FROM WS_MSG.
        02  LINE 21 COL 1 PIC X(50) FROM WS_INVITE.
        02  LINE 22 COL 1 VALUE "> ".
        02  LINE 22 COL 3 PIC X USING WS_CHOICE.
   
PROCEDURE DIVISION USING LS_QUIT.
    MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.
    DISPLAY STDSCREEN.
    MOVE 1 TO LS_QUIT.
EXIT PROGRAM.
END PROGRAM UTIL.

