# Banking Mainframe Project
- **Overview**: A hybrid project integrating COBOL and Python for banking operations.
- **COBOL Component**: `CALCINTEREST.cob` (compiled to `CALCINTEREST.exe`) calculates interest on account balances, reading from `scripts\bin\account_data.ps` and writing to `scripts\bin\output.ps`.
- **Python Component**: `banking.py` manages a SQLite database (`data\bank.db`), handles transactions, generates reports (`data\report.txt`), and integrates COBOL output.
- **Features**:
  - Stores 20 accounts with balances and interest rates.
  - Supports deposits and withdrawals with transaction logging (`data\transactions.txt`).
  - Calculates total interest using COBOL and appends to the report.
- **Setup**:
  - Install GnuCOBOL and Python.
  - Compile COBOL: `cobc -x scripts\bin\CALCINTEREST.cob`.
  - Run: `python banking.py`.
- **Challenges**: Adapted from MVS TK5 by avoiding JCL, using local file handling instead.
