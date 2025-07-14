       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-INTEREST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO
           "C:\banking-mainframe-project\scripts\bin\account_data.ps"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  INPUT-RECORD          PIC X(20).  *> Adjust length based on max line length
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X VALUE "N".
           88  END-OF-FILE-SW     VALUE "Y".
       01  ACCOUNT-DATA.
           05  ACCT-ID            PIC 9(6).
           05  SPACE-FILLER1      PIC X.
           05  ACCT-BALANCE-STR   PIC X(7).    *> e.g., "1950.00"
           05  SPACE-FILLER2      PIC X.
           05  INTEREST-RATE-STR  PIC X(3).    *> e.g., "5.5"
       01  WORK-AREA.
           05  TEMP-BALANCE       PIC 9(11)V99.  *> Temporary field for scaled value
           05  ACCT-BALANCE-TEMP  PIC 9(7)V99.    *> Temporary field to hold full number
           05  ACCT-BALANCE REDEFINES ACCT-BALANCE-TEMP PIC 9(6)V99. *> Numeric with decimal
           05  TEMP-RATE          PIC 9(2)V9(2).
           05  RATE               REDEFINES TEMP-RATE PIC 9(2)V9(2).
       01  TIMEAT                PIC 9(2) VALUE 2.
       01  INTEREST              PIC 9(7)V9(2).
       PROCEDURE DIVISION.
       OPEN INPUT ACCOUNT-FILE.
       PERFORM UNTIL END-OF-FILE-SW
           READ ACCOUNT-FILE
               AT END MOVE "Y" TO END-OF-FILE
               NOT AT END
                   DISPLAY "Raw Input: '" INPUT-RECORD "'"  *> Show raw line
                   UNSTRING INPUT-RECORD DELIMITED BY SPACE
                       INTO ACCT-ID ACCT-BALANCE-STR INTEREST-RATE-STR
                   END-UNSTRING
                   DISPLAY "Parsed: ACCT-ID='" ACCT-ID "' BALANCE='"
                   ACCT-BALANCE-STR "' RATE='" INTEREST-RATE-STR "'"
                   MOVE FUNCTION NUMVAL(ACCT-BALANCE-STR) TO
                   TEMP-BALANCE  *> Convert string to number
                   MOVE FUNCTION NUMVAL(INTEREST-RATE-STR) TO TEMP-RATE
                   IF TEMP-BALANCE = 0 OR TEMP-RATE = 0 OR
                ACCT-BALANCE-STR = SPACES OR INTEREST-RATE-STR = SPACES
                   DISPLAY "Warning: Invalid data for Account: " ACCT-ID
                   ELSE
                       COMPUTE ACCT-BALANCE-TEMP = TEMP-BALANCE * 1000  *> Shift decimal
                       DISPLAY "Debug: ACCT-BALANCE-STR = " ACCT-BALANCE-STR
                       DISPLAY "Debug: TEMP-BALANCE = " TEMP-BALANCE
                       DISPLAY "ACCT-BALANCE = " ACCT-BALANCE
                       DISPLAY "Debug: INTEREST-RATE-STR = "
                       INTEREST-RATE-STR
                       DISPLAY "Debug: RATE = " RATE
           COMPUTE INTEREST = (ACCT-BALANCE * RATE * TIMEAT) / 100
           DISPLAY "Account: " ACCT-ID " Interest Calculated: " INTEREST
           END-IF
           END-READ
       END-PERFORM.
       CLOSE ACCOUNT-FILE.
       STOP RUN.
