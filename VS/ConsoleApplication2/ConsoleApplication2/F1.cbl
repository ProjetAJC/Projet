        IDENTIFICATION DIVISION.
        PROGRAM-ID. F1.
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
           77  WS_DESC4        PIC X(80).
           77  WS_LNPIL        PIC X(18).
           77  WS_LDATEDEP     PIC X(18).
           77  WS_LHDEP        PIC X(17).
           77  WS_LDATEARR     PIC X(18).
           77  WS_LHARR        PIC X(17).
           77  WS_LDEST        PIC X(18).
           77  WS_LTAVION      PIC X(18).
           77  WS_OPTIONS      PIC X(80).
           77  WS_OPTVAL       PIC X(80).
           77  WS_OPTBACK      PIC X(80).
           77  WS_OPTQUIT      PIC X(80).
           77  WS_MSG          PIC X(50).
           77  WS_INVITE       PIC X(50).
           77  WS_NAV          PIC X.
           77  WS_QUERY        PIC 9(3).
           77  WS_VALID        PIC X.
           77  WS_QUIT         PIC 9.
           01  WS_DATEDEP.
               05  WS_JOURDEP  PIC 9(2).
               05  WS_MOISDEP  PIC 9(2).
               05  WS_ANDEP    PIC 9(4).
           01  WS_HDEP.
               05  WS_HEUREDEP PIC 9(2).
               05  WS_MINDEP   PIC 9(2).
           01  WS_DATEARR.
               05  WS_JOURARR  PIC 9(2).
               05  WS_MOISARR  PIC 9(2).
               05  WS_ANARR    PIC 9(4).
           01  WS_HARR.
               05  WS_HEUREARR PIC 9(2).
               05  WS_MINARR   PIC 9(2).
           77  WS_DEST         PIC X(25).
           77  WS_TAVION       PIC X(2).
   
           LINKAGE SECTION.
           77  LS_QUIT         PIC 9.
   
           SCREEN SECTION.
           01  CLRSCREEN BLANK SCREEN.

           01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
               02  LINE 3  COL 2 PIC 9(2) FROM WS_CURR_DAY.
               02  LINE 3  COL 4 VALUE "/".
               02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_MONTH.
               02  LINE 3  COL 7 VALUE "/".
               02  LINE 3  COL 8 PIC 9(4) FROM WS_CURR_YEAR.
               02  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
               02  LINE 3  COL 63 PIC X(25) FROM WS_FUNC.
               02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
               02  LINE 8  COL 2 PIC X(80) FROM WS_DESC1.
               02  LINE 9  COL 2 PIC X(80) FROM WS_DESC2.
               02  LINE 10 COL 2 PIC X(80) FROM WS_DESC3.
               02  LINE 11 COL 2 PIC X(80) FROM WS_DESC4.
               02  LINE 15 COL 2 PIC X(80) FROM WS_OPTIONS.
               02  LINE 16 COL 2 PIC X(80) FROM WS_OPTBACK.
               02  LINE 17 COL 2 PIC X(80) FROM WS_OPTQUIT.
               02  LINE 18 COL 2 PIC X USING WS_NAV.
               02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
               02  LINE 20 COL 2 PIC X(50) FROM WS_MSG.
               02  LINE 21 COL 2 PIC X(50) FROM WS_INVITE.
               02  LINE 22 COL 2 PIC 9(3) USING WS_QUERY.
       
           01  ENTRYSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
               02  LINE 3  COL 2 PIC 9(2) FROM WS_CURR_DAY.
               02  LINE 3  COL 4 VALUE "/".
               02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_MONTH.
               02  LINE 3  COL 7 VALUE "/".
               02  LINE 3  COL 8 PIC 9(4) FROM WS_CURR_YEAR.
               02  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
               02  LINE 3  COL 63 PIC X(25) FROM WS_FUNC.
               02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
               02  LINE 8  COL 2 PIC X(18) FROM WS_LNPIL.
               02  LINE 8  COL 21 PIC 9(3) FROM WS_QUERY.
               02  LINE 9  COL 2 PIC X(18) FROM WS_LDATEDEP.
               02  LINE 9  COL 21 PIC 9(2) USING WS_JOURDEP.
               02  LINE 9  COL 23 VALUE "/".
               02  LINE 9  COL 24 PIC 9(2) USING WS_MOISDEP.
               02  LINE 9  COL 26 VALUE "/".
               02  LINE 9  COL 27 PIC 9(4) USING WS_ANDEP.
               02  LINE 9  COL 49 PIC X(17) FROM WS_LHDEP.
               02  LINE 9  COL 67 PIC 9(2) USING WS_HEUREDEP.
               02  LINE 9  COL 69 VALUE ":".
               02  LINE 9  COL 70 PIC 9(2) USING WS_MINDEP.
               02  LINE 10 COL 2 PIC X(18) FROM WS_LDATEARR.
               02  LINE 10 COL 21 PIC 9(2) USING WS_JOURARR.
               02  LINE 10 COL 23 VALUE "/".
               02  LINE 10 COL 24 PIC 9(2) USING WS_MOISARR.
               02  LINE 10 COL 26 VALUE "/".
               02  LINE 10 COL 27 PIC 9(4) USING WS_ANARR.
               02  LINE 10 COL 49 PIC X(17) FROM WS_LHARR.
               02  LINE 10 COL 67 PIC 9(2) USING WS_HEUREARR.
               02  LINE 10 COL 69 VALUE ":".
               02  LINE 10 COL 70 PIC 9(2) USING WS_MINARR.
               02  LINE 11 COL 2 PIC X(18) FROM WS_LDEST.
               02  LINE 11 COL 21 PIC X(25) USING WS_DEST.
               02  LINE 12 COL 2 PIC X(18) FROM WS_LTAVION.
               02  LINE 12 COL 21 PIC X(2) USING WS_TAVION.
               02  LINE 14 COL 2 PIC X(80) FROM WS_OPTIONS.
               02  LINE 15 COL 2 PIC X(80) FROM WS_OPTVAL.
               02  LINE 16 COL 2 PIC X(80) FROM WS_OPTBACK.
               02  LINE 17 COL 2 PIC X(80) FROM WS_OPTQUIT.
               02  LINE 18 COL 2 PIC X USING WS_NAV.
               02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
               02  LINE 20 COL 2 PIC X(50) FROM WS_MSG.

        PROCEDURE DIVISION USING LS_QUIT.
        PRINCIPAL SECTION.
           PERFORM INIT_VALUES_F1.
           DISPLAY CLRSCREEN.
           DISPLAY ENTRYSCREEN.
           ACCEPT ENTRYSCREEN.
   
           EXIT PROGRAM.

        INIT_VALUES_F1.
           MOVE "DEPOT PLAN DE VOL" TO WS_FUNC.
           MOVE "-------------------------------------------------------
      --------------------------" TO WS_LINE.
           MOVE "Page de depot des plans de vol." TO WS_DESC1.
           MOVE "    1 - Saisissez le numero du pilote." TO WS_DESC2.
           MOVE "    2 - Entrez les donnees du plan de vol." TO WS_DESC3.
           MOVE "    3 - Validez les informations." TO WS_DESC4.
           MOVE "Options de navigation :" TO WS_OPTIONS.
           MOVE "    V - Valider les informations." TO WS_OPTVAL.
           MOVE "    M - Retourner au menu principal." TO WS_OPTBACK.
           MOVE "    Q - Quitter l'application." TO WS_OPTQUIT.
           MOVE SPACES TO WS_MSG.
           MOVE "Veuillez saisir le numero du pilote." TO WS_INVITE.
           MOVE SPACES TO WS_NAV.
           MOVE ZEROES TO WS_QUERY.
           MOVE SPACES TO WS_VALID.
           MOVE "Numero du pilote :" TO WS_LNPIL.
           MOVE "Date de depart   :" TO WS_LDATEDEP.
           MOVE "Date d'arrivee   :" TO WS_LDATEARR.
           MOVE "Destination      :" TO WS_LDEST.
           MOVE "Type d'avion     :" TO WS_LTAVION.
           MOVE "Heure de depart :" TO WS_LHDEP.
           MOVE "Heure d'arrivee :" TO WS_LHARR.
           MOVE ZEROES TO WS_DATEDEP.
           MOVE ZEROES TO WS_HDEP.
           MOVE ZEROES TO WS_DATEARR.
           MOVE ZEROES TO WS_HARR.
           MOVE SPACES TO WS_DEST.
           MOVE SPACES TO WS_TAVION.
           MOVE 1 TO WS_QUIT.
           MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.