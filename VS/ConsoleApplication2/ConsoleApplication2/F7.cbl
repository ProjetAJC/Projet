       IDENTIFICATION DIVISION.
       PROGRAM-ID. F7.

       DATA DIVISION.
           WORKING-STORAGE SECTION.    
           COPY "WS-TEMPLATE.cpy" IN TEMPLATE.
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
		
            77 WS-APP PIC X(20) VALUE "F7".
            77 WS-FUNC PIC X(20) VALUE "F7".
            77 WS-MSG PIC X(60).
            77 WS-INVITE PIC X(60).
            77 WS-CHOIX PIC X(20).
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
            77 PERIODE-DEP PIC 9(6).
            77 PERIODE-ARR PIC 9(6).
	
           SCREEN SECTION.
           
           01 DS-CHOIX-PERIODE.
               02 LINE 7 COL 10 VALUE "P�riode du".
               02 LINE 7 COL 21 FROM PERIODE-DEP.
               02 LINE 7 COL 28 VALUE "au".
               02 LINE 7 COL 31 FROM PERIODE-ARR.
              
           01 AS-CHOIX-PERIODE REQUIRED UNDERLINE.
               02 LINE 7 COL 21 TO PERIODE-DEP.
               02 LINE 7 COL 31 TO PERIODE-ARR.
           
           01 DS-RECAP-AVION.
           
           
           01 DS-RECAP-LINE.
           
           
           01 RECAP.
             02 PIC X(30) FROM "P�riode du " LINE 7 COL 10.
           01 TABLEAU.
             02 LINE 4 COL 1 VALUE "Num�ro Avion".
             02 LINE 4 COL 10 TO N-AVION.
       
       COPY "SCREEN-TEMPLATE.cpy" IN TEMPLATE.

       PROCEDURE DIVISION.
       DEBUT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
           PERFORM NEW-SCREEN.
           DISPLAY DS-CHOIX-PERIODE.
           ACCEPT AS-CHOIX-PERIODE.
           PERFORM SELECT-VOLS.
           PERFORM AFFICHE-FACTURE.
        
       COPY "PROC-TEMPLATE.cpy" IN TEMPLATE.

       SELECT-VOLS.
      * sql command

       AFFICHE-FACTURE.
           MOVE "Facturation des vols" TO WS-FUNC.
      *    PERFORM NEW-SCREEN.
           MOVE "n(page suivante), p(page pr�c�dente), v(payer)" 
               TO WS-INVITE.
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

	        DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	        ADD 1 TO NB-PAGE.

	        MOVE 1 TO VSTART.
	        MOVE 6 TO VSTOP.

           PERFORM UNTIL QUIT=1
               PERFORM REFRESH
               MOVE 10 TO K
               DISPLAY DS-CHOIX-PERIODE
               DISPLAY "Dates" LINE 8 COL 6
               DISPLAY "Dep." LINE 9 COL 3
               DISPLAY "Arr." LINE 9 COL 14
               DISPLAY "Destination" LINE 9 COL 25
               DISPLAY "Compteurs hor." LINE 8 COL 51
               DISPLAY "Dep." LINE 9 COL 51
               DISPLAY "Arr." LINE 9 COL 58
               DISPLAY "Nb h." LINE 9 COL 65
               DISPLAY "Cout" LINE 9 COL 72
               DISPLAY "Affichage des Vols" LINE 18 COL 3
               DISPLAY VSTART LINE 18 COL 22
               DISPLAY "�" LINE 18 COL 25
               DISPLAY VSTOP LINE 18 COL 28
               DISPLAY " (page /" NB-PAGE ")"
               DISPLAY "Total vols avion:" LINE 18 COL 57
               DISPLAY "" NB-VOL ""

               PERFORM VARYING J FROM VSTART BY 1 UNTIL J>VSTOP
                   ADD 1 TO K
                   IF NB-VOL>=J
                       DISPLAY DATEDEB(J) LINE K COL 3
                       DISPLAY DATEFIN(J) LINE K COL 14
                       DISPLAY DESTIN(J) LINE K COL 25
                       DISPLAY CPTDEP(J) LINE K COL 51
                       DISPLAY CPTARR(J) LINE K COL 58 
                       DISPLAY NBHVOL(J) LINE K COL 65
                       DISPLAY COUTVOL(J) LINE K COL 72
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
                       MOVE "Premi�re page atteinte" TO WS-MSG
                   END-IF
               END-IF
               DISPLAY CLRSCREEN
           END-PERFORM.
	
       STOP RUN.
       END PROGRAM F7.

