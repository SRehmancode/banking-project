       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-INTEREST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  PRINCIPAL    PIC 9(5) VALUE 1000.
           01  RATE         PIC 9(2)V9(2) VALUE 5.5.
           01  TIMEAT       PIC 9(2) VALUE 2.
           01  INTEREST     PIC 9(6)V9(2).
       PROCEDURE DIVISION.
           COMPUTE INTEREST = (PRINCIPAL * RATE * TIMEAT) / 100.
           DISPLAY "Interest Calculated: " INTEREST.
           STOP RUN.
