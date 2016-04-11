       IDENTIFICATION DIVISION.
       PROGRAM-ID. F3.
       AUTHOR. JEREMIE PAGNAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
           COPY "WS-TEMPLATE.cpy" IN TEMPLATE.
           77 VALID-CRE PIC 9 VALUE 0.
           77 WS-RETRY PIC 9 VALUE 3.
           77 WS-APP PIC X(20) VALUE "F3".
           77 WS-FUNC PIC X(20) VALUE "F3".
           77 WS-MSG PIC X(60).
           77 WS-INVITE PIC X(60).
      *    77 WS-CHOIX PIC X(20) VALUE SPACES.
       
           01 WS-CHOIX PIC X.
               88 WS-CHOIX-MENU VALUE "1", "2", "3", "4", "5", "6".
               88 WS-QUITTER VALUE "Q", "q".
               88 WS-VALIDER VALUE "Y", "y", "O", "o".
               88 WS-ANNULER VALUE "N", "n".

           01 PILOTE.
	          02 NUMPIL-PILOTE PIC 9(6).
		      02 NOM-PILOTE PIC X(20).
		      02 PRENOM-PILOTE PIC X(10).
		      02 ADRESSE-PILOTE PIC X(50).
		      02 CP-PILOTE PIC 9(5).
		      02 VILLE-PILOTE PIC X(50).
		      02 PAYS-PILOTE PIC X(50).
		      02 NBHVOL-PILOTE PIC 9(6).
              02 ETAT-PILOTE PIC AAA.
           
           01 ADRESSE.
              02 ID-ADRESSE PIC 9(6).
              02 NUMVOIE PIC X(50).
              02 CP PIC 9(5).
              02 VILLE PIC X(20).
              02 PAYS PIC X(20)

	       77 VALIDE PIC X.
	       77 NUM-PILOTE-MAJ PIC 9(3).
	       77 DEL-PILOTE-MAJ PIC 9(3).


	       01 FPILOTE.
		        02 PILOTE OCCURS 5.
			        03 NUMPIL PIC 9(3).
			        03 NOM PIC X(20).
			        03 PRENOM PIC X(10).
			        03 ADRESSE PIC X(50).
			        03 NBHVOL PIC 9(6).
			        03 ETATPIL PIC AAA.
           
      *    EXEC SQL
      *        INCLUDE Pilote
      *    END-EXEC.
      *    
      *    EXEC SQL BEGIN DECLARE SECTION
      *    END-EXEC.
      *    
      *    EXEC SQL END DECLARE SECTION
      *    END-EXEC.
           
       LINKAGE SECTION.
           77  LS_QUIT         PIC 9.
        
       SCREEN SECTION.
      *    01 CLRSCREEN BLANK SCREEN.
      *    01 STDSCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 2.
      *         02 LINE 1 COL 1 VALUE "A".
           
           01 DS-MENU.
               02 LINE 9  COL 5 "1. Afficher la liste des pilotes".
               02 LINE 10 COL 5 "2. Ajouter un pilote".
               02 LINE 11 COL 5 "3. Modifier un pilote".
               02 LINE 12 COL 5 "4. Supprimer un pilote".
               02 LINE 13 COL 5 "5. Revenir au menu principal".
               02 LINE 14 COL 5 "6. Quitter le programme".
               02 LINE 16 COL 5 PIC X TO WS-CHOIX.

           01 SS-CRE-PILOTE.
               02 LINE 7  COL 3 VALUE "Nom: ".
               02 LINE 8  COL 3 VALUE "Prénom: ".
               02 LINE 9  COL 3 VALUE "Adresse: ".
               02 LINE 10 COL 3 VALUE "CP: ".
               02 LINE 11 COL 3 VALUE "Ville: ".
               02 LINE 12 COL 3 VALUE "Pays: ".
               02 LINE 13 COL 3 VALUE "Compteur de vol: ".
      *        02 LINE 14 COL 1 "Brevet: ".
               02 LINE 16 COL 3 VALUE "Valider ? (y/n)".
           
           01 AS-CRE-PILOTE REQUIRED UNDERLINE.
               02 LINE 7  COL 20 PIC X(20) TO NOM-PILOTE.
               02 LINE 8  COL 20 PIC X(10) TO PRENOM-PILOTE.
               02 LINE 9  COL 20 PIC X(50) TO ADRESSE-PILOTE.
               02 LINE 10 COL 20 PIC 9(5)  TO CP-PILOTE.
               02 LINE 11 COL 20 PIC X(50) TO VILLE-PILOTE.
               02 LINE 12 COL 20 PIC X(50) TO PAYS-PILOTE.
               02 LINE 13 COL 20 PIC 9(6)  TO NBHVOL-PILOTE.
               02 LINE 16 COL 20 PIC X     TO WS-CHOIX.
           
           01 DS-MAJ-PILOTE.
               02 LINE 7  COL 3 FROM "Nom: ".
               02 LINE 8  COL 3 "Prénom: ".
               02 LINE 9  COL 3 "Adresse: ".
               02 LINE 10 COL 3 VALUE "CP: ".
               02 LINE 11 COL 3 VALUE "Ville: ".
               02 LINE 12 COL 3 VALUE "Pays: ".
               02 LINE 16 COL 3 VALUE "Valider ? (y/n)".

           01 AS-MAJ-PILOTE  REQUIRED UNDERLINE.
               02 LINE 7  COL 20 USING NOM-PILOTE.
               02 LINE 8  COL 20 USING PRENOM-PILOTE.
               02 LINE 9  COL 20 USING ADRESSE-PILOTE.
               02 LINE 10 COL 20 USING CP-PILOTE.
               02 LINE 11 COL 20 USING VILLE-PILOTE.
               02 LINE 12 COL 20 USING PAYS-PILOTE.
               02 LINE 16 COL 20 PIC X TO WS-CHOIX.
           
           COPY "SCREEN-TEMPLATE.cpy" IN TEMPLATE.
		
       PROCEDURE DIVISION USING LS_QUIT.
       

       DEBUT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
           PERFORM MENU.
      *    MOVE 'prenom' TO PRENOM-PILOTE.
      *    MOVE 'nom' TO NOM-PILOTE.
      *    MOVE 1 TO NUMPIL-PILOTE.
      *    MOVE 'adresse' TO ADRESSE-PILOTE.
       
      *NEW-SCREEN.
      *    PERFORM CLEAR-MSGS.
      *    DISPLAY CLRSCREEN.
      *    DISPLAY SS-STDSCREEN.
      *
      *REFRESH.
      *    PERFORM DISPLAY SS-STDSCREEN.
      *
      *CLEAR-MSGS.
      *    MOVE SPACES TO WS-MSG.
      *    MOVE SPACES TO WS-INVITE.
       
       COPY "PROC-TEMPLATE.cpy" IN TEMPLATE.

       BACK-OR-QUIT.
           MOVE "Retourner au Menu ou Quitter" TO WS-INVITE.
           PERFORM REFRESH.
           ACCEPT WS-CHOIX.
           IF WS-QUITTER
               STOP RUN
           ELSE 
               PERFORM MENU
           END-IF.
           
       
       MENU.
           MOVE "Menu" TO WS-FUNC.
           PERFORM NEW-SCREEN.
           DISPLAY DS-MENU.
           ACCEPT DS-MENU.
           PERFORM UNTIL WS-RETRY = 0
      *        DISPLAY SS-STDSCREEN
      *        ACCEPT SS-STDSCREEN
      *        ACCEPT WS-CHOIX.
               IF WS-CHOIX-MENU OR WS-QUITTER
	               EVALUATE WS-CHOIX
	                  WHEN "1"
			            PERFORM LISTE-PILOTE
		              WHEN "2"
			            PERFORM CRE-PILOTE
		              WHEN "3"
			            PERFORM MAJ-PILOTE
		              WHEN "4"
			            PERFORM DEL-PILOTE
                      WHEN "5"
                        EXIT PROGRAM
                      WHEN "6"
                        STOP RUN
		              WHEN OTHER 
			            PERFORM MENU
	               END-EVALUATE
                   MOVE "" TO WS-CHOIX
               ELSE
                   SUBTRACT 1 FROM WS-RETRY
                   MOVE "Choix non valide !" TO WS-MSG
               END-IF
           END-PERFORM.
	
       LISTE-PILOTE.
           STOP RUN.


       CRE-PILOTE.
           PERFORM NEW-SCREEN.
	       MOVE "ARL" TO ETAT-PILOTE.
           DISPLAY SS-CRE-PILOTE.
           ACCEPT AS-CRE-PILOTE.
           MOVE "ARL" TO ETAT-PILOTE. 
           IF WS-CHOIX = "y"
      *        PERFORM APPLY-CRE-PILOTE
               MOVE "Pilote créé" to WS-MSG
           ELSE
               MOVE "Pilote non créé" to WS-MSG
           END-IF.
           MOVE SPACES TO WS-CHOIX.
           PERFORM REFRESH.
           PERFORM BACK-OR-QUIT.
           PERFORM MENU.
	
	
       MAJ-PILOTE.
           PERFORM NEW-SCREEN.
           MOVE "Modifier un pilote" TO WS-FUNC.
           MOVE "Numero du pilote à modifier" TO WS-MSG.
           PERFORM REFRESH.
           
           ACCEPT NUM-PILOTE-MAJ LINE 22 COL 10.
           
      *    Recuperer les infos du pilote
           
           DISPLAY DS-MAJ-PILOTE.
           ACCEPT AS-MAJ-PILOTE.

           IF WS-CHOIX = "y"
      *        PERFORM APPLY-MAJ-PILOTE
               MOVE "Pilote modifié" to WS-MSG
           ELSE
               MOVE "Pilote non modifié" to WS-MSG
           END-IF.
           
           MOVE SPACES TO WS-CHOIX.
           PERFORM REFRESH.
           PERFORM BACK-OR-QUIT.
           PERFORM MENU.

  
       DEL-PILOTE.
           DISPLAY CLRSCREEN.     
           MOVE "Supprimer un pilote" to WS-FUNC
           MOVE "Pilote à supprimer:" to WS-MSG.
           DISPLAY SS-STDSCREEN.
           ACCEPT DEL-PILOTE-MAJ LINE 22 COL 10.
           PERFORM MENU.
    

       APPLY-CRE-PILOTE.
      *    EXEC SQL
      *        INSERT INTO ADRESSE(...)
      *        VALUES (
      *        
      *        SELECT ID-ETAT
      *        FROM ...
      *        WHERE ... = "A"
      *        
      *        INSERT INTO PILOTE(...)
      *        VALUES (NOM-PILOTE, PRENOM-PILOTE, ID-ADRESSE, 
      *        NBHVOL-PILOTE, ID-ETAT)
      *        
      *    END-EXEC.
           
           
       APPLY-MAJ-PILOTE.
      *    sql command
           
       APPLY-DEL-PILOTE.
      *    sql command

       END PROGRAM F3.