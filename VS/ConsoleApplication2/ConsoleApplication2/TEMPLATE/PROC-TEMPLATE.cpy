      * PROC-TEMPLATE 
       
       NEW-SCREEN.
           PERFORM CLEAR-MSGS.
           DISPLAY CLRSCREEN.
           DISPLAY SS-STDSCREEN.
       
       REFRESH.
           DISPLAY SS-STDSCREEN.
       
       CLEAR-MSGS.
           MOVE SPACES TO WS-MSG.
           MOVE SPACES TO WS-INVITE.