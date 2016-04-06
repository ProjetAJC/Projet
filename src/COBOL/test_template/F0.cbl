IDENTIFICATION DIVISION.
PROGRAM-ID. F0.
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
    77  WS_FUNC     PIC X(25)   VALUE "LISTE AVIONS/PILOTES".
    77  WS_LINE     PIC X(80)   VALUE "________________________________________________________________________________".
    77  WS_MTITLE   PIC X(80)   VALUE "Veuillez choisir parmis les options suivantes : ".
    77  WS_MBACK    PIC X(80)   VALUE "    M - Retourner au menu principal.".
    77  WS_MQUIT    PIC X(80)   VALUE "    Q - Quitter l'application.".
    77  WS_MSG      PIC X(50)   VALUE SPACES.
    77  WS_INVITE   PIC X(50)   VALUE SPACES.
    77  WS_CHOICE   PIC X       VALUE SPACES.
    77  WS_CONTINUE PIC 9.
    LINKAGE SECTION.
    77  LS_CONTINUE PIC 9.
    SCREEN SECTION.

PROCEDURE DIVISION USING LS_CONTINUE.
PRINCIPAL SECTION.
PGM_F0.
	CALL 'TEMPLATE' USING WS-STUDENT-ID, WS-STUDENT-NAME.
    MOVE FUNCTION CURRENT-DATE TO WS_CURR_DATE_FIELDS.
    MOVE 1 TO WS_CONTINUE.
    PERFORM UNTIL WS_CONTINUE = 0
        ACCEPT STDSCREEN
        EVALUATE WS_CHOICE
            WHEN "M" 
                MOVE SPACES TO WS_CHOICE
                MOVE 0 TO WS_CONTINUE 
            WHEN "Q" 
                MOVE SPACES TO WS_CHOICE
                MOVE 0 TO WS_CONTINUE, LS_CONTINUE
            WHEN OTHER 
                MOVE SPACES TO WS_CHOICE
                MOVE "Option invalide." TO WS_MSG
        END-EVALUATE
    END-PERFORM.
END_F0.
    STOP RUN.