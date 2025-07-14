       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-INTEREST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO
           "C:\banking-mainframe-project\scripts\bin\account_data.ps"
           ORGANIZATION IS LINE SEQUENTIAL
           RECORD DELIMITER IS STANDARD-1.  *> Ensures line breaks
           SELECT OUTPUT-FILE ASSIGN TO
           "C:\banking-mainframe-project\scripts\bin\output.ps"
           ORGANIZATION IS LINE SEQUENTIAL
           RECORD DELIMITER IS STANDARD-1.  *> Ensures line breaks
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE
           RECORD CONTAINS 20 CHARACTERS.  *> Match input line length
       01  INPUT-RECORD          PIC X(20).  *> Adjust length based on max line length
       FD  OUTPUT-FILE
           RECORD CONTAINS 80 CHARACTERS.  *> Match input line length
       01  OUTPUT-RECORD          PIC X(80).
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
           05  ACCT-BALANCE       PIC 9(7)V99. *> Numeric with decimal
           05  RATE               PIC 9(2)V99.
           05  INTEREST-AMOUNT    PIC 9(7)V99.
       01  TIMEAT                PIC 9(2) VALUE 2.
       01  WORK-RECORD           PIC X(80) VALUE SPACES.
       01  EDITED-FIELDS.
           05  EDITED-BALANCE     PIC Z(6)9.99.
           05  EDITED-RATE        PIC Z(2)9.99.
           05  EDITED-INTEREST    PIC Z(6)9.99.
       PROCEDURE DIVISION.
       OPEN INPUT ACCOUNT-FILE
            OUTPUT OUTPUT-FILE.
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
                   ACCT-BALANCE  *> Convert string to number
                   MOVE FUNCTION NUMVAL(INTEREST-RATE-STR) TO RATE
                   IF ACCT-BALANCE = 0 OR RATE = 0 OR
                ACCT-BALANCE-STR = SPACES OR INTEREST-RATE-STR = SPACES
                   MOVE "Warning: Invalid data for Account: "
                        TO WORK-RECORD
                    STRING WORK-RECORD ACCT-ID DELIMITED BY SIZE
                        INTO WORK-RECORD
                    MOVE WORK-RECORD TO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
                   ELSE
              COMPUTE INTEREST-AMOUNT =
                                    (ACCT-BALANCE * RATE * TIMEAT) / 100
                    MOVE ACCT-BALANCE TO EDITED-BALANCE
                    MOVE RATE TO EDITED-RATE
                    MOVE INTEREST-AMOUNT TO EDITED-INTEREST
                    MOVE SPACES TO WORK-RECORD  *> Clear before new string
              STRING "Account: " ACCT-ID
                     " Balance: " EDITED-BALANCE
                     " Rate: "   EDITED-RATE
                     " Interest: " EDITED-INTEREST
                     DELIMITED BY SIZE INTO WORK-RECORD
                     MOVE WORK-RECORD TO OUTPUT-RECORD
                     WRITE OUTPUT-RECORD  *> Ensure line break
           END-IF
           END-READ
       END-PERFORM.
       CLOSE ACCOUNT-FILE OUTPUT-FILE.
       STOP RUN.
