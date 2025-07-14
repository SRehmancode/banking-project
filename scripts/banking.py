import sqlite3
import os
import subprocess
from datetime import datetime

# Use portable file paths
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DB_PATH = os.path.join(BASE_DIR, '..', 'data', 'bank.db')
TRANS_PATH = os.path.join(BASE_DIR, '..', 'data', 'transactions.txt')
REPORT_PATH = os.path.join(BASE_DIR, '..', 'data', 'report.txt')
COBOL_INPUT_PATH = os.path.join(BASE_DIR, 'bin', 'account_data.ps')
COBOL_EXE_PATH = os.path.join(BASE_DIR, 'bin', 'CALCINTEREST.exe')
COBOL_OUTPUT_PATH = os.path.join(BASE_DIR, 'bin', 'output.ps')

# Start the database
conn = sqlite3.connect(DB_PATH)
cursor = conn.cursor()
cursor.execute('''CREATE TABLE IF NOT EXISTS ACCOUNTS
                 (ACCOUNT_ID INTEGER PRIMARY KEY, BALANCE REAL, INTEREST_RATE REAL, LAST_TRANSACTION_DATE TEXT)''')
cursor.execute('''CREATE TABLE IF NOT EXISTS TRANSACTIONS
                 (TRANSACTION_ID INTEGER PRIMARY KEY AUTOINCREMENT,
                  ACCOUNT_ID INTEGER, TRANS_TYPE TEXT, AMOUNT REAL, STATUS TEXT, TRANSACTION_DATE TEXT,
                  FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS (ACCOUNT_ID))''')

# Clear existing data (optional, remove if you want to keep existing data)
cursor.execute("DELETE FROM ACCOUNTS")
conn.commit()

# Insert 20 accounts
accounts_data = [
    (100001, 1000.00, 5.5, '2025-06-01'), (100002, 2000.00, 5.5, '2025-06-02'),
    (100003, 3000.00, 5.5, '2025-06-03'), (100004, 4000.00, 5.5, '2025-06-04'),
    (100005, 5000.00, 5.5, '2025-06-05'), (100006, 6000.00, 5.5, '2025-06-06'),
    (100007, 7000.00, 5.5, '2025-06-07'), (100008, 8000.00, 5.5, '2025-06-08'),
    (100009, 9000.00, 5.5, '2025-06-09'), (100010, 1500.00, 5.5, '2025-06-10'),
    (100011, 2500.00, 5.5, '2025-06-11'), (100012, 3500.00, 5.5, '2025-06-12'),
    (100013, 4500.00, 5.5, '2025-06-13'), (100014, 5500.00, 5.5, '2025-06-14'),
    (100015, 6500.00, 5.5, '2025-06-15'), (100016, 7500.00, 5.5, '2025-06-16'),
    (100017, 8500.00, 5.5, '2025-06-17'), (100018, 9500.00, 5.5, '2025-06-18'),
    (100019, 1100.00, 5.5, '2025-06-19'), (100020, 2100.00, 5.5, '2025-06-20')
]
try:
    cursor.execute("PRAGMA table_info(ACCOUNTS)")
    print("ACCOUNTS table schema:", cursor.fetchall())
    print("accounts_data:", accounts_data)
    cursor.executemany('INSERT OR IGNORE INTO ACCOUNTS VALUES (?, ?, ?, ?)', accounts_data)
    conn.commit()
    print("Accounts inserted successfully")
except sqlite3.Error as e:
    print(f"Error inserting accounts: {e}")

# Handle transactions
def process_transaction(acct_id, trans_type, amount):
    try:
        cursor.execute('SELECT BALANCE FROM ACCOUNTS WHERE ACCOUNT_ID = ?', (acct_id,))
        result = cursor.fetchone()
        status = ''
        transaction_date = datetime.now().strftime('%Y-%m-%d')
        if result:
            balance = result[0]
            if trans_type == 'D':
                balance += amount
                cursor.execute('UPDATE ACCOUNTS SET BALANCE = ?, LAST_TRANSACTION_DATE = ? WHERE ACCOUNT_ID = ?', 
                               (balance, transaction_date, acct_id))
                status = 'Deposit OK'
            elif trans_type == 'W':
                if balance >= amount:
                    balance -= amount
                    cursor.execute('UPDATE ACCOUNTS SET BALANCE = ?, LAST_TRANSACTION_DATE = ? WHERE ACCOUNT_ID = ?', 
                                   (balance, transaction_date, acct_id))
                    status = 'Withdrawal OK'
                else:
                    status = 'No Funds'
            else:
                status = 'Invalid Type'
        else:
            status = 'Account Not Found'
        cursor.execute('INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANS_TYPE, AMOUNT, STATUS, TRANSACTION_DATE) VALUES (?, ?, ?, ?, ?)', 
                       (acct_id, trans_type, amount, status, transaction_date))
        conn.commit()
        with open(TRANS_PATH, 'a') as f:
            f.write(f'Account: {acct_id}, Type: {trans_type}, Amount: {amount}, Status: {status}, Date: {transaction_date}\n')
        print(f'Status: {status}')
    except sqlite3.Error as e:
        print(f"Database error in transaction: {e}")
        conn.rollback()

# Make a report
def generate_report():
    try:
        cursor.execute('SELECT ACCOUNT_ID, BALANCE, INTEREST_RATE, LAST_TRANSACTION_DATE FROM ACCOUNTS')
        with open(REPORT_PATH, 'w') as f:
            f.write('End of Day Report\n')
            total_balance = 0
            for row in cursor.fetchall():
                f.write(f'Account: {row[0]} Bal: {row[1]:.2f} Rate: {row[2]}% Last Trans: {row[3]}\n')  # Changed to .2f for two decimals
                total_balance += row[1]
            f.write(f'Total Balance: {total_balance:.2f}\n')  # Changed to .2f for two decimals
    except sqlite3.Error as e:
        print(f"Error generating report: {e}")

# View transactions
def view_transactions():
    try:
        with open(TRANS_PATH, 'r') as f:
            print("\nTransaction History:")
            print(f.read())
    except FileNotFoundError:
        print("\nNo transaction history yet.")

# COBOL integration with total interest
def calculate_interest():
    try:
        # Write account data to input.ps
        with open(COBOL_INPUT_PATH, 'w') as f:
            cursor.execute('SELECT ACCOUNT_ID, BALANCE, INTEREST_RATE FROM ACCOUNTS')
            for row in cursor.fetchall():
                f.write(f"{row[0]:06d} {row[1]:07.2f} {row[2]:03.1f}\n")
        # Run the COBOL program
        result = subprocess.run([COBOL_EXE_PATH], capture_output=True, text=True, check=True)
        if result.returncode == 0:
            # Read the output from output.ps
            interest_values = []
            with open(COBOL_OUTPUT_PATH, 'r') as f:
                for line in f:
                    if "Account:" in line:
                        parts = line.split()
                        interest = float(parts[7])  # Interest is the 8th element (index 7)
                        interest_values.append(interest)
            total_interest = sum(interest_values) if interest_values else 0.0
            # Append to report
            with open(REPORT_PATH, 'a') as f:
                f.write("COBOL Interest Calculations:\n")
                with open(COBOL_OUTPUT_PATH, 'r') as output_file:
                    f.write(output_file.read())
                f.write(f"\nTotal Interest Calculated: {total_interest:.2f}\n")
        else:
            print(f"COBOL execution failed: {result.stderr}")
    except FileNotFoundError as e:
        print(f"File error: {e}")
    except subprocess.CalledProcessError as e:
        print(f"COBOL process error: {e}")
    except Exception as e:
        print(f"Error calling COBOL: {e}")

# Main program flow
if __name__ == "__main__":
    calculate_interest()
    while True:
        view_transactions()
        acct_id = input('Enter Account ID (0 to exit): ')
        if acct_id == '0':
            break
        if not acct_id.isdigit():
            print("Account ID must be a number")
            continue
        trans_type = input('Enter Type (D=Deposit, W=Withdrawal): ').upper()
        if trans_type not in ('D', 'W'):
            print("Invalid transaction type. Use D or W.")
            continue
        try:
            amount = float(input('Enter Amount: '))
            if amount <= 0:
                print("Amount must be positive")
                continue
            process_transaction(int(acct_id), trans_type, amount)
        except ValueError:
            print('Invalid input. Use numbers for amount.')
    generate_report()
    calculate_interest()
    conn.close()