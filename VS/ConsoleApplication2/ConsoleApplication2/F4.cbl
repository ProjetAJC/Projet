       IDENTIFICATION DIVISION.
       PROGRAM-ID. F4.
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

           01 AVION.
	          02 CODAV PIC 9(3).
              02 CPTHORAV PIC 9(6).
              02 CPTINTER PIC 9(3).
              02 INFOS PIC X(50).
              02 ETATAV PIC A.
              02 CODTYP PIC XX.
              
	       77 VALIDE PIC X.
	       77 NUM-AVION-MAJ PIC 9(3).
	       77 DEL-AVION-MAJ PIC 9(3).

           
      *    EXEC SQL
      *        INCLUDE Avion
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
               02 LINE 9  COL 5 "1. Afficher la liste des avions".
               02 LINE 10 COL 5 "2. Ajouter un avion".
               02 LINE 11 COL 5 "3. Modifier un avion".
               02 LINE 12 COL 5 "4. Supprimer un avion".
               02 LINE 13 COL 5 "5. Revenir au menu principal".
               02 LINE 14 COL 5 "6. Quitter le programme".
               02 LINE 16 COL 5 PIC X TO WS-CHOIX.
  

           01 DS-CRE-AVION.
      *        02 LINE 7  COL 1 VALUE "Code: ".
               02 LINE 8  COL 3 VALUE "Compteur horaire: ".
               02 LINE 9  COL 3 VALUE "Infos: ".
               02 LINE 10 COL 3 VALUE "Type de l'avion: ".
               02 LINE 12 COL 3 VALUE "Valider ? (y/n)".
           
           01 AS-CRE-AVION REQUIRED UNDERLINE.
      *        02 LINE 7  COL 20 PIC 9(6) TO CODAV.
               02 LINE 8  COL 20 PIC 9(6) TO CPTHORAV.
               02 LINE 9  COL 20 PIC X(50)  TO INFOS.
               02 LINE 10 COL 20 PIC XX TO CODTYP.
               02 LINE 12 COL 20 PIC X     TO WS-CHOIX.
           
           01 DS-MAJ-AVION.
               02 LINE 7  COL 3 FROM "Code de l'avion: ".
               02 LINE 8  COL 3 "Infos: ".
               02 LINE 10 COL 3 VALUE "Valider ? (y/n)".

           01 AS-MAJ-AVION  REQUIRED UNDERLINE.
               02 LINE 7  COL 20 FROM CODAV.
               02 LINE 8  COL 20 USING INFOS.
               02 LINE 10 COL 20 PIC X TO WS-CHOIX.
           
           COPY "SCREEN-TEMPLATE.cpy" IN TEMPLATE.
		
       PROCEDURE DIVISION USING LS_QUIT.
       DEBUT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
           PERFORM MENU.
      *    MOVE 'prenom' TO PRENOM-AVION.
      *    MOVE 'nom' TO NOM-AVION.
      *    MOVE 1 TO NUMPIL-AVION.
      *    MOVE 'adresse' TO ADRESSE-AVION.
       
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
           MOVE "ReTOurner au Menu ou Quitter (Q)" TO WS-INVITE.
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
           PERFORM UNTIL WS-RETRY = 0
               PERFORM REFRESH
               DISPLAY DS-MENU
               ACCEPT DS-MENU
      *        DISPLAY SS-STDSCREEN
      *        ACCEPT SS-STDSCREEN
               IF WS-CHOIX-MENU OR WS-QUITTER
	               EVALUATE WS-CHOIX
	                  WHEN "1"
			            PERFORM LISTE-AVION
		              WHEN "2"
			            PERFORM CRE-AVION
		              WHEN "3"
			            PERFORM MAJ-AVION
		              WHEN "4"
			            PERFORM DEL-AVION
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
                   MOVE "Choix non valide ! " TO WS-MSG
               END-IF
           END-PERFORM.
           STOP RUN.
	
       LISTE-AVION.
           STOP RUN.


       CRE-AVION.
           PERFORM NEW-SCREEN.
	       MOVE "E" TO ETATAV.
           MOVE 0   TO CPTINTER.
           DISPLAY DS-CRE-AVION.
           ACCEPT AS-CRE-AVION.
           IF WS-CHOIX = "y"
      *        PERFORM APPLY-CRE-AVION
               MOVE "Avion créé" TO WS-MSG
           ELSE
               MOVE "Avion non créé" TO WS-MSG
           END-IF.
           MOVE SPACES TO WS-CHOIX.
           PERFORM REFRESH.
           PERFORM BACK-OR-QUIT.
           PERFORM MENU.
	
	
       MAJ-AVION.
           PERFORM NEW-SCREEN.
           MOVE "Modifier un avion" TO WS-FUNC.
           MOVE "Numero du avion à modifier" TO WS-MSG.
           PERFORM REFRESH.
           
           ACCEPT NUM-AVION-MAJ LINE 22 COL 10.
           
      *    Recuperer les infos du avion
           
           DISPLAY DS-MAJ-AVION.
           ACCEPT AS-MAJ-AVION.

           IF WS-CHOIX = "y"
      *        PERFORM APPLY-MAJ-AVION
               MOVE "Avion modifié" TO WS-MSG
           ELSE
               MOVE "Avion non modifié" TO WS-MSG
           END-IF.
           
           MOVE SPACES TO WS-CHOIX.
           PERFORM REFRESH.
           PERFORM BACK-OR-QUIT.
           PERFORM MENU.

  
       DEL-AVION.
           PERFORM NEW-SCREEN.
           MOVE "Supprimer un avion" TO WS-FUNC
           MOVE "Avion à supprimer:" TO WS-MSG.
           PERFORM REFRESH.
           ACCEPT DEL-AVION-MAJ LINE 22 COL 10.
           PERFORM MENU.
    

       APPLY-CRE-AVION.
      *    EXEC SQL
      *        INSERT INTO ADRESSE(...)
      *        VALUES (
      *        
      *        SELECT ID-ETAT
      *        FROM ...
      *        WHERE ... = "A"
      *        
      *        INSERT INTO AVION(...)
      *        VALUES (NOM-AVION, PRENOM-AVION, ID-ADRESSE, 
      *        NBHVOL-AVION, ID-ETAT)
      *        
      *    END-EXEC.
           
           
       APPLY-MAJ-AVION.
      *    sql command
           
       APPLY-DEL-AVION.
      *    sql command

       END PROGRAM F4.
