* F7
* All rights Reserved by Team Bioinfo
*
IDENTIFICATION DIVISION.
PROGRAM-ID F7.
AUTHOR. Jeremie.
*
ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 INPUT-OUTPUT SECTION.
  FILE-CONTROL.
	SELECT ### ASSIGN TO ###.
	###
*
DATA DIVISION.
 WORKING-STORAGE SECTION.
 FILE SECTION.
* Recuperation des donnees TODO  
  ###
  01 Facture.
    02 Lignes OCCURS ###.
      03 Dates.
        04 Depart PIC ###.
	    04 Arrivee PIC ###.
	  03 Dest PIC ###.
	  03 Compteurs_horaire.
	    04 Depart PIC ###.
		04 Arrivee PIC ###.
	  03 Nombre_heure_vol PIC ###.
	  03 Cout_vol PIC ###.
*
PROCEDURE DIVISION.
AFFICHAGE.
	DISPLAY "FACTURATION DES VOLS" .
	DISPLAY "Période du " ### "au" ###.
	DISPLAY "N° avion" ### "Tarif" ###.
* Boucle for, on parcourt les ligne de la facture et on les affiche 
	PERFORM UNTIL ###
		DISPLAY Depart(I) Arrivee(I) Dest(I) Depart(I) Arrivee(I) Nombre_heure_vol(I) Cout_vol(I).
	DISPLAY "Total vols avion " ###