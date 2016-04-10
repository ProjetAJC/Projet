        IDENTIFICATION DIVISION.
        PROGRAM-ID. F2.
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
    
      *     EXEC SQL
      *         INCLUDE SQLCA
      *     END-EXEC.
      *
      *     EXEC SQL
      *         INCLUDE AVIONS, VOLS, PILOTES, ETAT_PILOTE
      *     END-EXEC.
      *
      *     EXEC SQL BEGIN DECLARE SECTION
      *     END-EXEC.
      *         01  WS-VOL-REC.
      *             05  WS-VOL-ID           PIC 9(6).
      *             05  WS-VOL-CPTDEP       PIC 9(6).
      *             05  WS-VOL-CPTARR       PIC 9(6).
      *             05  WS-VOL-ETAT         PIC X.
      *         01  WS-AVION-REC.
      *             05  WS-AVION-ID         PIC 9(3).
      *             05  WS-AVION-CPTHOR     PIC 9(6).
      *             05  WS-AVION-CPTINT     PIC 9(6).
      *         01  WS-PILOTE-REC.
      *             05  WS-PILOTE-ID        PIC 9(3).
      *             05  WS-PILOTE-NBHVOL    PIC 9(6).
      *         01  WS-ETATPILOTE-REC.
      *             05  WS-ETATPILOTE-ID    PIC 9(2).
      *             05  WS-SANTE-PIL        PIC X.
      *             05  WS-FINANCES-PIL     PIC X.
      *             05  WS-DISPO-PIL        PIC X.
      *     EXEC SQL END DECLARE SECTION
      *     END-EXEC.

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
                02  LINE 3  COL 62 PIC X(25) FROM WS_FUNC.
                02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 8  COL 2 PIC X(80) FROM WS_DESC1.
                02  LINE 9  COL 2 PIC X(80) FROM WS_DESC2.
                02  LINE 10 COL 2 PIC X(80) FROM WS_DESC3.
                02  LINE 15 COL 2 PIC X(80) FROM WS_OPTIONS.
                02  LINE 16 COL 2 PIC X(80) FROM WS_OPTBACK.
                02  LINE 17 COL 2 PIC X(80) FROM WS_OPTQUIT.
                02  LINE 18 COL 2 PIC X USING WS_NAV.
                02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 20 COL 2 PIC X(50) FROM WS_MSG.
                02  LINE 21 COL 2 PIC X(50) FROM WS_INVITE.
                02  LINE 22 COL 2 PIC 9(3) USING WS_QUERY.
       
            01  VALSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                02  LINE 3  COL 2 PIC 9(2) FROM WS_CURR_DAY.
                02  LINE 3  COL 4 VALUE "/".
                02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_MONTH.
                02  LINE 3  COL 7 VALUE "/".
                02  LINE 3  COL 8 PIC 9(4) FROM WS_CURR_YEAR.
                02  LINE 3  COL 32 VALUE "GESTION AEROCLUB".
                02  LINE 3  COL 62 PIC X(25) FROM WS_FUNC.
                02  LINE 6  COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 8  COL 2 PIC X(80) FROM WS_RECAP.
                02  LINE 10 COL 2 PIC X(80) FROM WS_NVOL.
                02  LINE 11 COL 2 PIC X(80) FROM WS_HDEP.
                02  LINE 12 COL 2 PIC X(80) FROM WS_HARR.
                02  LINE 13 COL 2 PIC X(80) FROM WS_NAVION.
                02  LINE 13 COL 22 PIC 9(3) FROM WS_QUERY.
                02  LINE 14 COL 2 PIC X(80) FROM WS_CIVPILOTE.
                02  LINE 16 COL 2 PIC X(80) FROM WS_WARNING.
                02  LINE 19 COL 1 PIC X(80) FROM WS_LINE.
                02  LINE 20 COL 2 PIC X(50) FROM WS_MSG.
                02  LINE 21 COL 2 PIC X(50) FROM WS_INVITE.
                02  LINE 22 COL 2 PIC X USING WS_VALID.

        PROCEDURE DIVISION USING LS_QUIT.
        PRINCIPAL SECTION.
            PERFORM INIT_VALUES_F2.

            PERFORM UNTIL WS_QUIT = 0
                PERFORM REFRESH_SCREEN_F2
                IF WS_QUERY <> 0 THEN
                    PERFORM SQL_TREATMENT_F2
                ELSE
                    PERFORM NAV_TREATMENT_F2
                END-IF
                MOVE ZEROES TO WS_QUERY
            END-PERFORM.

            EXIT PROGRAM.

        INIT_VALUES_F2.
            MOVE "ENREGISTREMENT VOL" TO WS_FUNC.
            MOVE "------------------------------------------------------
      ---------------------------" TO WS_LINE.
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
            MOVE "La validation de ces informations entrainera la mise a
      - jour de la BDD" TO WS_WARNING.
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

        REFRESH_SCREEN_F2.
            DISPLAY CLRSCREEN.
            DISPLAY STDSCREEN.
            ACCEPT STDSCREEN.

        NAV_TREATMENT_F2.
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

        SQL_TREATMENT_F2.
            PERFORM SQL_QUERY_F2.
      *     IF SQL_CODE = 0 THEN
      *         PERFORM SQL_RECAP_F2
      *         EVALUATE WS_VALID
      *             WHEN "O"
      *                 PERFORM SQL_UPDATE_F2
      *             WHEN "N"
      *                 PERFORM INIT_VALUES_F2
      *                 DISPLAY CLRSCREEN
      *                 DISPLAY STDSCREEN
      *         END-EVALUATE.
      *         MOVE SPACES TO WS_VALID
      *     ELSE
      *         MOVE "Identifiant invalide." TO WS_MSG.
      *     END-IF.

        SQL_QUERY_F2.
      *     EXEC SQL
      *         SELECT  ID_VOL, CPTDEP_VOL, CPTARR_VOL, ETAT_VOL,
      *                 ID_AVION, CPTHOR_AVION, CPTINTER_AVION,
      *                 ID_PILOTE, NBHVOL_PILOTE, ID_ETATP,
      *                 SANTE_PILOTE, FINANCES_PILOTE, DISPO_PILOTE
      *         INTO    :WS-VOL-ID, :WS-VOL-CPTDEP, WS-VOL-CPTARR, WS-VOL-ETAT,
      *                 WS-AVION-ID, WS-AVION-CPTHORAV, WS-AVION-CPTINTER,
      *                 WS-PILOTE-ID, WS-PILOTE-NBHVOL, WS-ETATPILOTE-ID,
      *                 WS-SANTE-PIL, WS-FINANCES-PIL, WS-DISPO-PIL
      *         FROM    AVIONS INNER JOIN VOLS INNER JOIN PILOTES INNER JOIN ETAT_PILOTE
      *         ON      AVIONS.ID_AVION = VOLS.ID_AVION,
      *                 VOLS.ID_PILOTE = PILOTES.ID_PILOTE,
      *                 PILOTES.ID_ETATP = ETAT_PILOTE.ID_ETATP
      *         WHERE   ID_AVION = WS_QUERY
      *                 AND ETAT_VOL = 'D'
      *     END-EXEC.

        SQL_RECAP_F2.
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

        SQL_UPDATE_F2.
      *     MOVE 'T' TO WS-VOL-ETAT.
      *     MOVE 'L' TO WS-DISPOPILOTE.
      *     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-VOL-CPTARR.
      *     COMPUTE WS_TPSVOL = WS-VOL-CPTARR - WS-VOL-CPTDEP.
      *     ADD WS_TPSVOL TO WS-AVION-CPTHORAV.
      *     ADD WS_TPSVOL TO WS-AVION-CPTINTER.
      *     ADD WS_TPSVOL TO WS-PILOTE-NBHVOL.
      *     EXEC SQL
      *         UPDATE  VOLS
      *         SET     COMPTEUR_ARRIVEE = :WS-VOL-CPTARR,
      *                 ETAT-VOL = :WS-VOL-ETAT
      *         WHERE   N__VOL = :WS-VOL-ID
      *     END-EXEC.
      *     EXEC SQL
      *         UPDATE  AVIONS
      *         SET     COMPTEUR_HORAIRE = :WS-AVION-CPTHORAV
      *                 COMPTEUR_INTERMEDIAIRE = :WS-AVION-CPTINTER
      *         WHERE   N__AVION = :WS-AVION-ID
      *     END-EXEC.
      *     EXEC SQL
      *         UPDATE  PILOTES
      *         SET     NB_HEURES_VOL = :WS-PILOTE-NBHVOL
      *         WHERE   N__PILOTE = :WS-PILOTE-ID
      *     END-EXEC.
      *     EXEC SQL
      *         UPDATE  ETAT_PILOTE
      *         SET     DISPONIBILITE_PILOTE = :WS-DISPOPILOTE
      *         WHERE   N__ETAT_PILOTE = :WS-ETATPILOTE-ID
      *     END-EXEC.
      *     IF SQL_CODE = 0 THEN
      *         MOVE "Mise-a-jour de la base de donnees realisee 
      *         avec succes." TO WS_MSG
      *     ELSE
      *         MOVE "Erreur lors de la mise-a-jour de la base 
      *         de donnees." TO WS_MSG
      *     END-IF.
            PERFORM INIT_VALUES_F2.
            DISPLAY CLRSCREEN.
            DISPLAY STDSCREEN.
    