        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN.

        DATA DIVISION.
           WORKING-STORAGE SECTION.
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
            77 WS-APP PIC X(20) VALUE "F7".
            77 WS-FUNC PIC X(20) VALUE "F7".
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
            01 CLRSCREEN BLANK SCREEN.
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
        
        PROCEDURE DIVISION.
        AFFICHE-FACTURE.
            MOVE "n(page suivante), p(page précédente), v(payer)" TO WS-INVITE.
	        MOVE 8 TO NB-VOL.
            move 2 to NB-DE-PILOTE.

            move "pagnac" to NOM(1).
            move "nom2" to NOM(2).
            move "jeremie" to PRENOM(1).
            move "prenom2" to PRENOM(2).

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

            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
	        DIVIDE NB-VOL BY 6 GIVING NB-PAGE.
	        ADD 1 TO NB-PAGE.

	        MOVE 1 TO VSTART.
	        MOVE 6 TO VSTOP.

	        PERFORM UNTIL QUIT=1
		        MOVE 10 TO K
                move NOM(NUM-CUR-PILOTE) to CUR-NOM-PILOTE
                move PRENOM(NUM-CUR-PILOTE) to CUR-PRENOM-PILOTE
		        DISPLAY "N avion" LINE 7 COL 3
                DISPLAY "Nom" LINE 8 COL 3
                DISPLAY "Prenom" LINE 8 COL 10
		        DISPLAY "N vol" LINE 8 COL 30
		        DISPLAY "Depart" LINE 8 COL 45
		        DISPLAY "Temps vol" LINE 8 COL 60
		        DISPLAY "Total vols avion:" LINE 18 COL 57
		        DISPLAY "" NB-VOL ""
                display SS-INFOS-PILOTES
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
                        if NB-DE-PILOTE <= NUM-CUR-PILOTE
			                MOVE "Derniere page atteinte" TO WS-MSG
                        else
                            MOVE "pilote suivant" TO WS-MSG
                            MOVE 1 TO VSTART
	                        MOVE 6 TO VSTOP
                            add 1 to NUM-CUR-PILOTE
                        end-if
                        
			        END-IF
		        END-IF
		        IF USER-VAL='p'
			        IF VSTART > 1
				        SUBTRACT 1 FROM VSTART
				        SUBTRACT 1 FROM VSTOP
			        ELSE
                       if NUM-CUR-PILOTE = 1
                       		MOVE "Première page atteinte" TO WS-MSG
                       else
                            MOVE 1 TO VSTART
	                        MOVE 6 TO VSTOP
                            subtract 1 FROM NUM-CUR-PILOTE
                            move "pilote precedent" to WS-MSG
			        END-IF
		        END-IF
		        DISPLAY CLRSCREEN
		        CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX
	        END-PERFORM.
	
        STOP RUN.
        END PROGRAM MAIN.



        IDENTIFICATION DIVISION.
        PROGRAM-ID. UTIL.

        DATA DIVISION.
            WORKING-STORAGE SECTION.
            01  WS-CURR-DATE-FIELDS.
                05  WS-CURR-DATE.
                    10  WS-CURR-YEAR    PIC  9(4).
                    10  WS-CURR-MONTH   PIC  9(2).
                    10  WS-CURR-DAY     PIC  9(2).
                05  WS-CURR-TIME.
                    10  WS-CURR-HOUR    PIC  9(2).
                    10  WS-CURR-MINUTE  PIC  9(2).
                    10  WS-CURR-SECOND  PIC  9(2).
                    10  WS-CURR-MS      PIC  9(2).
                05  WS-DIFF-FROM-GMT    PIC S9(4).
            77  WS-APP      PIC X(25)   VALUE "DEFAULT".
            77  WS-FUNC     PIC X(25)   VALUE "DEFAULT".
            77  WS-MSG      PIC X(60)   VALUE "DEFAULT".
            77  WS-INVITE   PIC X(60)   VALUE "DEFAULT".
            77  WS-CHOIX    PIC X(25)   VALUE "DEFAULT".
            77  WS-LINE     PIC X(80)   VALUE "--------------------------------------------------------------------------------".
            77  WS-MTITLE   PIC X(80)   VALUE "Veuillez choisir parmis les options suivantes : ".
            77  WS-MBACK    PIC X(80)   VALUE "    M - Retourner au menu principal.".
            77  WS-MQUIT    PIC X(80)   VALUE "    Q - Quitter l'application.".
            77  WS-CHOICE   PIC X       VALUE SPACES.
            77  WS-CONTINUE PIC 9.
            77  WSOULIGNE   PIC X(60)   VALUE ALL "-".

            LINKAGE SECTION.
            01 LS-APP PIC X(20).
            01 LS-FUNC PIC X(20).
            01 LS-MSG PIC X(60).
            01 LS-INVITE PIC X(60).
            01 LS-CHOIX PIC X(20).
    
            SCREEN SECTION.
            01  STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
                02  LINE 3  COL 5 PIC 9(2) FROM WS-CURR-DAY.
                02  LINE 3  COL 7 VALUE "/".
                02  LINE 3  COL 8 PIC 9(2) FROM WS-CURR-MONTH.
                02  LINE 3  COL 10 VALUE "/".
                02  LINE 3  COL 11 PIC 9(4) FROM WS-CURR-YEAR.
                02  LINE 3  COL 30 VALUE "GESTION AEROCLUB".
                02  LINE 3  COL 50 PIC X(25) FROM WS-FUNC.
                02  LINE 6  COL 1 PIC X(60) FROM WSOULIGNE.
                02  LINE 19  COL 1 PIC X(60) FROM WSOULIGNE.
                02  LINE 20 COL 1 PIC X(60) FROM WS-MSG.
                02  LINE 21 COL 1 PIC X(60) FROM WS-INVITE.
                02  LINE 22 COL 1 VALUE "Choix :".
                02  LINE 22 COL 9 PIC X USING WS-CHOICE.
   
        PROCEDURE DIVISION USING LS-APP, LS-FUNC, LS-MSG, LS-INVITE, LS-CHOIX.
            MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
            MOVE LS-APP TO WS-APP.
            MOVE LS-FUNC TO WS-FUNC.
            MOVE LS-MSG TO WS-MSG.
            MOVE LS-INVITE TO WS-INVITE.
            MOVE LS-CHOIX TO WS-CHOIX.
            DISPLAY STDSCREEN.
    
        EXIT PROGRAM.
        END PROGRAM UTIL.

