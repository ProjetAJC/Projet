      * PROC-TEMPLATE 
       MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-FIELDS.
       
       NEW-SCREEN.
           PERFORM CLEAR-MSGS.
           DISPLAY CLRSCREEN.
           DISPLAY SS-STDSCREEN.
       
       REFRESH.
           PERFORM DISPLAY SS-STDSCREEN.
       
       CLEAR-MSGS.
           MOVE SPACES TO WS-MSG.
           MOVE SPACES TO WS-INVITE.