        IDENTIFICATION DIVISION.
        PROGRAM-ID. F3.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

        DATA DIVISION.
        FILE SECTION.
        WORKING-STORAGE SECTION.

            77 WS-APP PIC X(20) VALUE "F3".
            77 WS-FUNC PIC X(20) VALUE "F3".
            77 WS-MSG PIC X(60).
            77 WS-INVITE PIC X(60).
            77 WS-CHOIX PIC X(20).

	        01 PILOTE.
		        02 NUMPIL-PILOTE PIC 9(6).
		        02 NOM-PILOTE PIC X(20).
		        02 PRENOM-PILOTE PIC X(10).
		        02 ADRESSE-PILOTE PIC X(50).
                02 ETAT-PILOTE PIC AAA.

	        77 VALIDE PIC X.
	        77 NUM-PILOTE-MAJ PIC 9(3).
	        77 DEL-PILOTE-MAJ PIC 9(3).

	        01 NEW-PILOTE.
		        02 NEW-NOM PIC X(20).
		        02 NEW-PRENOM PIC X(10).
		        02 NEW-ADRESSE PIC X(50).
		        02 NEW-NBH-VOL PIC 9(6).
		        02 NEW-BREVET PIC 9.
		        02 NEW-ETATPIL PIC AAA VALUE "ARL".
	
	        01 MAJ-PILOTE.
		        02 MAJ-NOM PIC X(20).
		        02 MAJ-PRENOM PIC X(10).
		        02 MAJ-ADRESSE PIC X(50).
		        02 MAJ-NBH-VOL PIC 9(6).
		        02 MAJ-BREVET PIC 9.
		
	        01 FPILOTE.
		        02 PILOTE OCCURS 5.
			        03 NUMPIL PIC 9(3).
			        03 NOM PIC X(20).
			        03 PRENOM PIC X(10).
			        03 ADRESSE PIC X(50).
			        03 NBHVOL PIC 9(6).
			        03 ETATPIL PIC AAA.

        SCREEN SECTION.
	        01 CLRSCREEN BLANK SCREEN.
	        01 STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
		        02 LINE 1 COL 1 VALUE "A".
            01 SS-MENU-MAJ-PILOTE.
               02 LINE 7 COL 1 FROM "Nom: ".
               02 USING NOM-PILOTE.
               02 LINE 8 COL 1 "Prénom: ".
               02 USING PRENOM-PILOTE.
               02 LINE 9 COL 1 "Adresse: ".
               02 USING ADRESSE-PILOTE.
               02 LINE 10 COL 1 "Brevet: ".
               02 LINE 22 COL 10 TO WS-CHOIX.

		
        PROCEDURE DIVISION.
        DEBUT.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.

	        MOVE 'prenom' TO PRENOM-PILOTE.
	        MOVE 'nom' TO NOM-PILOTE.
	        MOVE 1 TO NUMPIL-PILOTE.
	        MOVE 'adresse' TO ADRESSE-PILOTE.

        MENU.
            DISPLAY CLRSCREEN.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
      *     DISPLAY STDSCREEN.
            DISPLAY "F3" LINE 5 COL 5.
            DISPLAY "1. Afficher la liste des pilotes" LINE 9 COL 5.
            DISPLAY "2. Ajouter un pilote" LINE 10 COL 5.
            DISPLAY "3. Modifier un pilote" LINE 11 COL 5.
            DISPLAY "4. Supprimer un pilote" LINE 12 COL 5.
            DISPLAY "5. Revenir au menu principal" LINE 13 COL 5.
            DISPLAY "6. Quitter le programme" LINE 14 COL 5.
            ACCEPT WS-CHOIX line 22 col 15.
	        EVALUATE WS-CHOIX
		        WHEN "1"
			        PERFORM LISTE-PILOTE
		        WHEN "2"
			        PERFORM MENU-CRE-PILOTE
		        WHEN "3"
			        PERFORM MENU-MAJ-PILOTE
		        WHEN "4"
			        PERFORM MENU-DEL-PILOTE
		        WHEN OTHER 
			        PERFORM MENU
	        END-EVALUATE.
	

        LISTE-PILOTE.
            STOP RUN.


        MENU-CRE-PILOTE.
            DISPLAY CLRSCREEN.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
	        MOVE "ARL" TO ETAT-PILOTE.
            DISPLAY "Ajouter un pilote" LINE 1 COL 30.
            DISPLAY "Nom: " LINE 9 COL 1.
            DISPLAY "Prénom: " LINE 10 COL 1.
            DISPLAY "Adresse: " LINE 11 COL 1.
            DISPLAY "Brevet: " LINE 12 COL 1.
            ACCEPT NEW-NOM LINE 9 COL 15.
            ACCEPT NEW-PRENOM LINE 10 COL 15.
            ACCEPT NEW-ADRESSE LINE 11 COL 15.
            ACCEPT NEW-BREVET LINE 12 COL 15.
            DISPLAY "Valider la création d'un pilote. (y/n)" LINE 21 COL 1.
      *     ACCEPT VALIDE LINE 22.
            accept VALIDE line 22.
            IF VALIDE = 'y'
                move "Pilote créé" to WS-MSG
      *         DISPLAY "Pilote créé" LINE 21 COL 1
            ELSE
                IF VALIDE = 'n'
                    move "Pilote non créé" to WS-MSG
      *             DISPLAY "Pilote non créé" LINE 21 COL 1
                ELSE
                    move "Erreur, commande invalide" to WS-MSG
      *             DISPLAY "Erreur, commande invalide" LINE 20 COL 1
                END-IF
            END-IF.
      *     accept VALIDE.
            PERFORM MENU.
	
	
        MENU-MAJ-PILOTE.
            DISPLAY CLRSCREEN.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
            DISPLAY "Modifier un pilote" LINE 1 COL 30.
    
            DISPLAY "Pilote à modifier:" LINE 21 COL 1.
            ACCEPT NUM-PILOTE-MAJ LINE 22 COL 10.
            
            DISPLAY SS-MENU-MAJ-PILOTE.
            accept SS-MENU-MAJ-PILOTE.
            
            move "Valider la modification d'un pilote. (y/n)" TO WS-MSG.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.

      *     DISPLAY "Valider la modification d'un pilote. (y/n)" LINE 21 COL 1.
            ACCEPT VALIDE LINE 22 COL 9.
      *     accept WS-CHOIX.
            IF VALIDE = 'y'
                move "Pilote modifié" to WS-MSG
            ELSE
                IF VALIDE = 'n'
                    move "Pilote non modifié" to WS-MSG
                ELSE
                    move "Erreur, commande invalide" to WS-MSG
                END-IF
            END-IF.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
            PERFORM MENU.

  


        MENU-DEL-PILOTE.
            DISPLAY CLRSCREEN.
            move "Supprimer un pilote" to WS-FUNC
            move "Pilote à supprimer:" to WS-MSG.
            CALL 'UTIL' USING WS-APP, WS-FUNC, WS-MSG, WS-INVITE, WS-CHOIX.
            ACCEPT DEL-PILOTE-MAJ LINE 22 COL 10.
            PERFORM MENU.
    



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
                02  LINE 22 COL 9 TO WS-CHOICE.
   
        PROCEDURE DIVISION USING LS-APP, LS-FUNC, LS-MSG, LS-INVITE, LS-CHOIX.
            MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
            MOVE LS-APP TO WS-APP.
            MOVE LS-FUNC TO WS-FUNC.
            MOVE LS-MSG TO WS-MSG.
            MOVE LS-INVITE TO WS-INVITE.
      *     MOVE LS-CHOIX TO WS-CHOIX.
            DISPLAY STDSCREEN.
    
        EXIT PROGRAM.
        END PROGRAM UTIL.