       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUTDD.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUTDD.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD          PIC X(20).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD         PIC X(80).
       WORKING-STORAGE SECTION.
       01  ACCOUNT-DATA.
           05  ACCT-ID           PIC 9(6).
           05  FILLER            PIC X.
           05  ACCT-BALANCE      PIC 9(7)V99.
           05  FILLER            PIC X.
           05  INT-RATE          PIC 9(2)V9.
       01  EOF-SWITCH             PIC X VALUE "N".
           88  EOF-SWITCH-TRUE    VALUE "Y".
       01  TIMEAT                PIC 9(2) VALUE 2.
       01  INTEREST              PIC 9(7)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.
           READ INPUT-FILE
               AT END MOVE 'Y' TO EOF-SWITCH
           END-READ.
           PERFORM UNTIL EOF-SWITCH-TRUE
               MOVE INPUT-RECORD TO ACCOUNT-DATA
               IF ACCT-ID IS NUMERIC AND ACCT-BALANCE IS NUMERIC
                  AND INT-RATE IS NUMERIC
                   COMPUTE INTEREST = (ACCT-BALANCE * INT-RATE * TIMEAT) / 100
                   STRING "Account: " ACCT-ID
                          " Balance: " ACCT-BALANCE
                          " Rate: " INT-RATE
                          " Interest: " INTEREST
                          DELIMITED BY SIZE INTO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
               ELSE
                   STRING "Error: Invalid data for Account: " ACCT-ID
                          DELIMITED BY SIZE INTO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
               END-IF
               READ INPUT-FILE
                   AT END MOVE 'Y' TO EOF-SWITCH
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE OUTPUT-FILE.
           STOP RUN.
