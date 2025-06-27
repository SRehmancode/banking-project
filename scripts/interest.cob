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
       01  INPUT-RECORD          PIC X(14).  *> Adjust length based on max line length
       WORKING-STORAGE SECTION.
       01  END-OF-FILE            PIC X VALUE "N".
           88  END-OF-FILE-SW     VALUE "Y".
       01  ACCOUNT-DATA.
           05  ACCT-ID            PIC 9(6).
           05  COMMA-FILLER       PIC X.
           05  ACCT-BALANCE-STR   PIC X(7).    *> e.g., "1950.00"
       01  WORK-AREA.
           05  TEMP-BALANCE       PIC 9(11)V99.  *> Temporary field for scaled value
           05  ACCT-BALANCE-TEMP  PIC 9(8)V99.    *> Temporary field to hold full number
           05  ACCT-BALANCE REDEFINES ACCT-BALANCE-TEMP PIC 9(5)V99. *> Numeric with decimal
       01  TIMEAT                PIC 9(2) VALUE 2.
       01  INTEREST              PIC 9(6)V9(2).
       01  RATE                  PIC 9(2)V9(2) VALUE 5.5.
       PROCEDURE DIVISION.
       OPEN INPUT ACCOUNT-FILE.
       PERFORM UNTIL END-OF-FILE-SW
           READ ACCOUNT-FILE
           AT END MOVE "Y" TO END-OF-FILE
           NOT AT END
            UNSTRING INPUT-RECORD DELIMITED BY ","
                INTO ACCT-ID ACCT-BALANCE-STR
            END-UNSTRING
           MOVE FUNCTION NUMVAL(ACCT-BALANCE-STR) TO TEMP-BALANCE  *> Convert string to number
           DISPLAY "Debug: ACCT-BALANCE-STR = " ACCT-BALANCE-STR
           DISPLAY "Debug: TEMP-BALANCE = " TEMP-BALANCE
           COMPUTE ACCT-BALANCE-TEMP = TEMP-BALANCE * 1000  *> Shift decimal
           DISPLAY "ACCT-BALANCE = " ACCT-BALANCE
           COMPUTE INTEREST = (ACCT-BALANCE * RATE * TIMEAT) / 100
           DISPLAY "Account: " ACCT-ID " Interest Calculated: " INTEREST
           END-READ
       END-PERFORM.
       CLOSE ACCOUNT-FILE.
       STOP RUN.
