      * PROC-TEMPLATE 
       
       NEW-SCREEN2.
           PERFORM CLEAR-MSGS.
           DISPLAY CLRSCREEN.
           DISPLAY SS-STDSCREEN.
       
       NEW-SCREEN.
           PERFORM CLEAR-MSGS.
           PERFORM REFRESH.
           
       REFRESH.
           DISPLAY CLRSCREEN.
           DISPLAY BANNIERE.
           DISPLAY AVIONG.
           DISPLAY AVIOND.
           DISPLAY CADRE.
           DISPLAY INSIDE-BANNIERE.
           DISPLAY FOOTER.
       
       CLEAR-MSGS.
           MOVE SPACES TO WS-MSG.
           MOVE SPACES TO WS-INVITE.