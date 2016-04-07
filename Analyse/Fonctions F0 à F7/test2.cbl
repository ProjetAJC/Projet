IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
   WORKING-STORAGE SECTION.
    77 a PIC 9.
    77 WS-APP PIC X(20) VALUE "F7".
    77 WS-FUNC PIC X(20) VALUE "F7".
    77 WS-MSG PIC X(60).
    77 WS-INVITE PIC X(60).
    77 WS-CHOIX PIC X(20).

	01 FVOL.
	  02 VOL OCCURS 8.
		03 NUMVOL PIC 9(6).
		03 DATEDEB PIC X(10).
		03 DATEFIN PIC X(10).
		03 CPTDEP PIC 9(6) VALUE ZEROES.
		03 CPTARR PIC 9(6) VALUE ZEROES.
		03 DESTIN PIC X(25) VALUE SPACES.
		03 ATATVOL PIC A.
		03 NUMAV PIC 9(3).
		03 NUMPIL PIC 9(3).
		03 NBHVOL PIC 9(6) VALUE ZEROES.
		03 COUTVOL PIC 9(6).
	
   01 WS-STUDENT-ID PIC 9(4) VALUE 1000.
   01 WS-STUDENT-NAME PIC A(15) VALUE 'Tim'.
   77 NB-VOL PIC 99.
    77 NB-PAGE PIC 99.
    77 I PIC 99 VALUE 1.
    77 J PIC 99 VALUE 1.
    77 VSTART PIC 99.
    77 VSTOP PIC 99.
    77 QUIT PIC 9 VALUE 0.
    77 USER-VAL PIC X.
    77 K PIC 9(3).
    77 TARIF PIC 9(3) VALUE 15.
    77 N-AVION PIC 9.
	
   SCREEN SECTION.
    01  CLRSCREEN BLANK SCREEN.
    01 B.
      02 PIC X(6) FROM "line 1" LINE 9 COL 1.
      02 PIC X(6) FROM "line 2" LINE 10 COL 1.
      02 PIC X(6) FROM "line 3" LINE 11 COL 1.
    01 C.
      02 PIC X(6) FROM "line 4" LINE 9 COL 1.
      02 PIC X(6) FROM "line 5" LINE 10 COL 1.
      02 PIC X(6) FROM "line 6" LINE 11 COL 1.
    01 RECAP.
        02 PIC X(30) FROM "Période du " LINE 7 COL 10.
    01 TABLEAU.
        02 LINE 4 COL 1 VALUE "Numéro Avion".
        02 LINE 4 COL 10 TO N-AVION.

PROCEDURE DIVISION.
AFFICHE-FACTURE.
    MOVE "n(page suivante), p(page précédente), v(payer)" TO WS-INVITE.
	MOVE 8 TO NB-VOL.

	MOVE 1 TO NUMVOL(1).
	MOVE 2 TO NUMVOL(2).
	MOVE 3 TO NUMVOL(3).
	MOVE 4 TO NUMVOL(4).
	MOVE 5 TO NUMVOL(5).
	MOVE 6 TO NUMVOL(6).
	MOVE 7 TO NUMVOL(7).
	MOVE 8 TO NUMVOL(8).

	MOVE "dest1" TO DESTIN(1).
	MOVE "dest2" TO DESTIN(2).
	MOVE "dest3" TO DESTIN(3).
	MOVE "dest4" TO DESTIN(4).
	MOVE "dest5" TO DESTIN(5).
	MOVE "dest6" TO DESTIN(6).
	MOVE "dest7" TO DESTIN(7).
	MOVE "dest8" TO DESTIN(8).

	MOVE 1 TO COUTVOL(1).
	MOVE 2 TO COUTVOL(2).
	MOVE 3 TO COUTVOL(3).
	MOVE 4 TO COUTVOL(4).
	MOVE 5 TO COUTVOL(5).
	MOVE 6 TO COUTVOL(6).
	MOVE 7 TO COUTVOL(7).
	MOVE 8 TO COUTVOL(8).

	MOVE 1 TO CPTDEP(1).
	MOVE 2 TO CPTDEP(2).
	MOVE 3 TO CPTDEP(3).
	MOVE 4 TO CPTDEP(4).
	MOVE 5 TO CPTDEP(5).
	MOVE 6 TO CPTDEP(6).
	MOVE 7 TO CPTDEP(7).
	MOVE 8 TO CPTDEP(8).

	MOVE 10 TO CPTARR(1).
	MOVE 20 TO CPTARR(2).
	MOVE 30 TO CPTARR(3).
	MOVE 40 TO CPTARR(4).
	MOVE 50 TO CPTARR(5).
	MOVE 60 TO CPTARR(6).
	MOVE 70 TO CPTARR(7).
	MOVE 80 TO CPTARR(8).

	SUBTRACT CPTDEP(1) FROM CPTARR(1) GIVING NBHVOL(1).
	SUBTRACT CPTDEP(2) FROM CPTARR(2) GIVING NBHVOL(2).
	SUBTRACT CPTDEP(3) FROM CPTARR(3) GIVING NBHVOL(3).
	SUBTRACT CPTDEP(4) FROM CPTARR(4) GIVING NBHVOL(4).
	SUBTRACT CPTDEP(5) FROM CPTARR(5) GIVING NBHVOL(5).
	SUBTRACT CPTDEP(6) FROM CPTARR(6) GIVING NBHVOL(6).
	SUBTRACT CPTDEP(7) FROM CPTARR(7) GIVING NBHVOL(7).

	MULTIPLY NBHVOL(1) BY TARIF GIVING COUTVOL(1).
	MULTIPLY NBHVOL(2) BY TARIF GIVING COUTVOL(2).
	MULTIPLY NBHVOL(3) BY TARIF GIVING COUTVOL(3).
	MULTIPLY NBHVOL(4) BY TARIF GIVING COUTVOL(4).
	MULTIPLY NBHVOL(5) BY TARIF GIVING COUTVOL(5).
	MULTIPLY NBHVOL(6) BY TARIF GIVING COUTVOL(6).
	MULTIPLY NBHVOL(7) BY TARIF GIVING COUTVOL(7).


	DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	ADD 1 TO NB-PAGE.

    CALL 'UTIL' USING WS-STUDENT-ID, WS-STUDENT-NAME, WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX..
    DISPLAY B.
    ACCEPT a.
	
	
	
	
	DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	ADD 1 TO NB-PAGE.

	MOVE 1 TO VSTART.
	MOVE 6 TO VSTOP.

	PERFORM UNTIL QUIT=1
		MOVE 7 TO K
		DISPLAY "Date" LINE 5 COL 3
		DISPLAY "Compteurs hor." LINE 5 COL 30
		DISPLAY "Depart" LINE 6 COL 1
		DISPLAY "Arrive" LINE 6 COL 12
		DISPLAY "Destination" LINE 6 COL 23
		DISPLAY "Depart" LINE 6 COL 49
		DISPLAY "Arrivee" LINE 6 COL 56
		DISPLAY "Nb heure" LINE 6 COL 64
		DISPLAY "Cout" LINE 6 COL 73
		DISPLAY "Affichage des Vols" LINE 14 COL 1
		DISPLAY VSTART LINE 14 COL 20
		DISPLAY "à" LINE 14 COL 23
		DISPLAY VSTOP LINE 14 COL 26
		DISPLAY " (page /" NB-PAGE ")"
		DISPLAY "Total vols avion:" LINE 14 COL 57
		DISPLAY "" NB-VOL ""
		
		DISPLAY "bas de page" LINE 16 COL 1

		PERFORM VARYING J FROM VSTART BY 1 UNTIL J>VSTOP
			ADD 1 TO K
			IF NB-VOL>=J
				DISPLAY DATEDEB(J) LINE K COL 1
				DISPLAY DATEFIN(J) LINE K COL 12
				DISPLAY DESTIN(J) LINE K COL 23
				DISPLAY CPTDEP(J) LINE K COL 49
				DISPLAY CPTARR(J) LINE K COL 56
				DISPLAY NBHVOL(J) LINE K COL 63
				DISPLAY COUTVOL(J) LINE K COL 70
			ELSE
				DISPLAY SPACES LINE K COL 1
			END-IF
		END-PERFORM
		ACCEPT USER-VAL
		IF USER-VAL=1
			MOVE 1 TO QUIT
		END-IF
		MOVE SPACES TO WS-MSG
		IF USER-VAL='n'
		    
			IF NB-VOL > VSTOP
				ADD 6 TO VSTART
				ADD 6 TO VSTOP
			ELSE
			    MOVE "Derniere page atteinte" TO WS-MSG
			END-IF
		END-IF
		IF USER-VAL='p'
			IF VSTART >= 7
				SUBTRACT 6 FROM VSTART
				SUBTRACT 6 FROM VSTOP
			ELSE
				MOVE "Première page atteinte" TO WS-MSG
			END-IF
		END-IF
		DISPLAY CLRSCREEN
		CALL 'UTIL' USING WS-STUDENT-ID, WS-STUDENT-NAME, WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX
	END-PERFORM.
	
	
STOP RUN.
END PROGRAM MAIN.

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
    77  WS-APP     PIC X(25)   VALUE "DEFAULT".
    77  WS-FUNC     PIC X(25)   VALUE "DEFAULT".
    77  WS-MSG     PIC X(60)   VALUE "DEFAULT".
    77  WS-INVITE     PIC X(60)   VALUE "DEFAULT".
    77  WS-CHOIX     PIC X(25)   VALUE "DEFAULT".
    77  WS_LINE     PIC X(80)   VALUE "________________________________________________________________________________".
    77  WS_MTITLE   PIC X(80)   VALUE "Veuillez choisir parmis les options suivantes : ".
    77  WS_MBACK    PIC X(80)   VALUE "    M - Retourner au menu principal.".
    77  WS_MQUIT    PIC X(80)   VALUE "    Q - Quitter l'application.".
    77  WS_MSG      PIC X(50)   VALUE SPACES.
    77  WS_INVITE   PIC X(50)   VALUE SPACES.
    77  WS_CHOICE   PIC X       VALUE SPACES.
    77  WS_CONTINUE PIC 9.
    77  WSOULIGNE   PIC X(60)   VALUE ALL "-".

   LINKAGE SECTION.
    01 LS-STUDENT-ID PIC 9(4).
    01 LS-STUDENT-NAME PIC A(15).
    01 LS-APP PIC X(20).
    01 LS-FUNC PIC X(20).
    01 LS-MSG PIC X(60).
    01 LS-INVITE PIC X(60).
    01 LS-CHOIX PIC X(20).
    
  SCREEN SECTION.
    01 SC.
        02 PIC 9(4) FROM WS_CURR_YEAR LINE 5 COL 5.
    01 HEADER.
        02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_DAY.
        02  LINE 3  COL 7 VALUE "/".
        02  LINE 3  COL 8 PIC 9(2) FROM WS_CURR_MONTH.
        02  LINE 3  COL 10 VALUE "/".
        02  LINE 3  COL 11 PIC 9(4) FROM WS_CURR_YEAR.
        02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
        02  LINE 3  COL 50 PIC X(25) FROM WS-FUNC.
        02  LINE 6  COL 1 PIC X(80) FROM WSOULIGNE.
    01 FOOTER.
        02  LINE 20 COL 1 PIC X(50) FROM WS_MSG.
        02  LINE 21 COL 1 PIC X(50) FROM WS_INVITE.
        02  LINE 22 COL 1 VALUE "> ".
        02  LINE 22 COL 3 PIC X USING WS_CHOICE.
    01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
        02  LINE 3  COL 5 PIC 9(2) FROM WS_CURR_DAY.
        02  LINE 3  COL 7 VALUE "/".
        02  LINE 3  COL 8 PIC 9(2) FROM WS_CURR_MONTH.
        02  LINE 3  COL 10 VALUE "/".
        02  LINE 3  COL 11 PIC 9(4) FROM WS_CURR_YEAR.
        02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
        02  LINE 3  COL 50 PIC X(25) FROM WS-FUNC.
        02  LINE 6  COL 1 PIC X(60) FROM WSOULIGNE.
        02  LINE 20 COL 1 PIC X(60) FROM WS-MSG.
        02  LINE 21 COL 1 PIC X(60) FROM WS-INVITE.
        02  LINE 22 COL 1 VALUE "Choix :".
        02  LINE 22 COL 9 PIC X USING WS_CHOICE.
   
PROCEDURE DIVISION USING LS-STUDENT-ID, LS-STUDENT-NAME, LS-APP, LS-FUNC, LS-MSG, LS-INVITE, LS-CHOIX.
    MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.
    MOVE LS-APP TO WS-APP.
    MOVE LS-FUNC TO WS-FUNC.
    MOVE LS-MSG TO WS-MSG.
    MOVE LS-INVITE TO WS-INVITE.
    MOVE LS-CHOIX TO WS-CHOIX.
    DISPLAY STDSCREEN.
    MOVE 1111 TO LS-STUDENT-ID.
EXIT PROGRAM.
END PROGRAM UTIL.

