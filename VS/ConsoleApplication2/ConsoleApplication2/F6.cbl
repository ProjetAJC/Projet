       IDENTIFICATION DIVISION.
       PROGRAM-ID. F6.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           
           COPY "WS-TEMPLATE.cpy" IN TEMPLATE.
           
	       01 FVOL.
	         02 VOL OCCURS 0 TO 99 TIMES DEPENDING ON NB-VOL.
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
            
           01 FPILOTE.
             02 PILOTE OCCURS 2.
               03 NUMPIL PIC 9(3).
               03 NOM PIC X(20).
               03 PRENOM PIC X(10).
               03 ADRESSE PIC X(50).
               03 NBHVOL-PILOTE PIC 9(6).
               03 ETATPIL PIC AAA.
               
            
           01 CUR-VOL.
             02 CUR-NUMVOL PIC 9(6).
		     02 CUR-DATEDEB PIC X(10).
		     02 CUR-DATEFIN PIC X(10).
		     02 CUR-CPTDEP PIC 9(6).
		     02 CUR-CPTARR PIC 9(6).
		     02 CUR-DESTIN PIC X(25).
		     02 CUR-ATATVOL PIC A.
		     02 CUR-NUMAV PIC 9(3).
		     02 CUR-NUMPIL PIC 9(3).
		     02 CUR-NBHVOL PIC 9(6).
		     02 CUR-COUTVOL PIC 9(6).
            
           01 CUR-PILOTE.
             02 CUR-NOM-PILOTE PIC X(20).
             02 CUR-PRENOM-PILOTE PIC X(10).

           77 NUM-CUR-PILOTE PIC 99 VALUE 1.
           77 WS-APP PIC X(20) VALUE "F6".
           77 WS-FUNC PIC X(20) VALUE "F6".
           77 WS-MSG PIC X(60).
           77 WS-INVITE PIC X(60).
           77 WS-CHOIX PIC X(20).
	       77 NB-DE-PILOTE PIC 99.
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

           01 RECAP.
             02 PIC X(30) FROM "Période du " LINE 7 COL 10.
           01 TABLEAU.
             02 LINE 4 COL 1 VALUE "Numéro Avion".
             02 LINE 4 COL 10 TO N-AVION.

      *     01 SS-AVION.
      *         02 LINE K COL 40 PIC 9(6) FROM CUR-AVION.

	       01 SS-LINE-TABLE.
             02 LINE K COL 30 PIC 9(6) FROM CUR-NUMVOL.
             02 LINE K COL 45 PIC 9(6) FROM CUR-DATEDEB.
             02 LINE K COL 60 PIC 9(6) FROM CUR-NBHVOL.
           01 SS-INFOS-PILOTES.
             02 LINE K COL 5 FROM CUR-NOM-PILOTE.
             02 LINE K COL 15 FROM CUR-PRENOM-PILOTE.
           
           01 DS-CHOIX-PERIODE.
           
           01 AS-CHOIX-PERIODE.
           
           
           01 DS-RECAP-PILOTE.
           
           01 DS-RECAP-AVION.
           
           01 DS-RECAP-LINE.
           
           COPY "SCREEN-TEMPLATE.cpy" IN TEMPLATE.

       PROCEDURE DIVISION.
       DEBUT.
           MOVE "Récapitulatif Pilotes" TO WS-FUNC.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
           PERFORM AFFICHE-FACTURE.
       
       
       
       TEST1.
           MOVE 8 TO NB-VOL.
           MOVE 2 to NB-DE-PILOTE.

           MOVE "pagnac" to NOM(1).
           MOVE "nom2" to NOM(2).
           MOVE "jeremie" to PRENOM(1).
           MOVE "prenom2" to PRENOM(2).

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

       
       COPY "PROC-TEMPLATE.cpy" IN TEMPLATE.
           
       AFFICHE-FACTURE.
           PERFORM NEW-SCREEN.
           MOVE "n(page suivante), p(page précédente), v(payer)" 
           TO WS-INVITE.
	       PERFORM TEST1.
	       DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	       ADD 1 TO NB-PAGE.

	       DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	       ADD 1 TO NB-PAGE.

	       MOVE 1 TO VSTART.
	       MOVE 6 TO VSTOP.

	       PERFORM UNTIL QUIT=1
		       MOVE 10 TO K
               MOVE NOM(NUM-CUR-PILOTE) to CUR-NOM-PILOTE
               MOVE PRENOM(NUM-CUR-PILOTE) to CUR-PRENOM-PILOTE
		       DISPLAY "N avion" LINE 7 COL 3
               DISPLAY "Nom" LINE 8 COL 3
               DISPLAY "Prenom" LINE 8 COL 10
		       DISPLAY "N vol" LINE 8 COL 30
		       DISPLAY "Depart" LINE 8 COL 45
		       DISPLAY "Temps vol" LINE 8 COL 60
		       DISPLAY "Total vols avion:" LINE 18 COL 57
		       DISPLAY "" NB-VOL ""
               DISPLAY SS-INFOS-PILOTES
		       PERFORM VARYING J FROM VSTART BY 1 UNTIL J>VSTOP
			       ADD 1 TO K
			       IF NB-VOL>=J
                       MOVE NUMVOL(J) to CUR-NUMVOL
	                   MOVE CPTDEP(J) TO CUR-CPTDEP
	                   MOVE CPTARR(J) TO CUR-CPTARR
			           MOVE NBHVOL(J) TO CUR-NBHVOL
		               MOVE COUTVOL(J) TO CUR-COUTVOL
				       DISPLAY SS-LINE-TABLE
			       END-IF
		       END-PERFORM
		       ACCEPT USER-VAL line 22 col 10
		       IF USER-VAL=1
			       MOVE 1 TO QUIT
		       END-IF
		       MOVE SPACES TO WS-MSG
                
		       IF USER-VAL='n'
			       IF NB-VOL > VSTOP
			           ADD 1 TO VSTART
			           ADD 1 TO VSTOP
			       ELSE
                       IF NB-DE-PILOTE <= NUM-CUR-PILOTE
			               MOVE "Derniere page atteinte" TO WS-MSG
                       ELSE
                           MOVE "pilote suivant" TO WS-MSG
                           MOVE 1 TO VSTART
	                       MOVE 6 TO VSTOP
                           ADD 1 to NUM-CUR-PILOTE
                       END-IF
			       END-IF
		       END-IF
		       IF USER-VAL='p'
			       IF VSTART > 1
				       SUBTRACT 1 FROM VSTART
				       SUBTRACT 1 FROM VSTOP
			       ELSE
                       IF NUM-CUR-PILOTE = 1
                           MOVE "Première page atteinte" TO WS-MSG
                       ELSE
                           MOVE 1 TO VSTART
	                       MOVE 6 TO VSTOP
                           SUBTRACT 1 FROM NUM-CUR-PILOTE
                           MOVE "pilote precedent" to WS-MSG
			           END-IF
		           END-IF
               END-IF
	  *         DISPLAY CLRSCREEN
               PERFORM NEW-SCREEN
	       END-PERFORM.
	
       STOP RUN.
       END PROGRAM F6.

