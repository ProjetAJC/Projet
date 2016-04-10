      * WS-TEMPLATE 
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

       77  WS-CONTINUE PIC 9.
       77  WSOULIGNE   PIC X(80)   VALUE ALL "-".
