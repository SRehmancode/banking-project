import sqlite3
import os
import subprocess

# Start the database
conn = sqlite3.connect('C:/banking-mainframe-project/data/bank.db')
cursor = conn.cursor()
cursor.execute('''CREATE TABLE IF NOT EXISTS ACCOUNTS
                 (ACCOUNT_ID INTEGER PRIMARY KEY, BALANCE REAL)''')
cursor.execute('''CREATE TABLE IF NOT EXISTS TRANSACTIONS
                 (ACCOUNT_ID INTEGER, TRANS_TYPE TEXT, AMOUNT REAL, STATUS TEXT)''')
cursor.execute('INSERT OR IGNORE INTO ACCOUNTS VALUES (123456, 1000.0), (789012, 5000.0)')
conn.commit()

# Handle transactions
def process_transaction(acct_id, trans_type, amount):
    cursor.execute('SELECT BALANCE FROM ACCOUNTS WHERE ACCOUNT_ID = ?', (acct_id,))
    result = cursor.fetchone()
    status = ''
    if result:
        balance = result[0]
        if trans_type == 'D':
            balance += amount
            cursor.execute('UPDATE ACCOUNTS SET BALANCE = ? WHERE ACCOUNT_ID = ?', (balance, acct_id))
            status = 'Deposit OK'
        elif trans_type == 'W':
            if balance >= amount:
                balance -= amount
                cursor.execute('UPDATE ACCOUNTS SET BALANCE = ? WHERE ACCOUNT_ID = ?', (balance, acct_id))
                status = 'Withdrawal OK'
            else:
                status = 'No Funds'
        else:
            status = 'Invalid Type'
    else:
        status = 'Account Not Found'
    cursor.execute('INSERT INTO TRANSACTIONS VALUES (?, ?, ?, ?)', (acct_id, trans_type, amount, status))
    conn.commit()
    print(f'Status: {status}')
    with open('C:/banking-mainframe-project/data/transactions.txt', 'a') as f:
        f.write(f'Account: {acct_id}, Type: {trans_type}, Amount: {amount}, Status: {status}\n')

# Make a report
def generate_report():
    cursor.execute('SELECT ACCOUNT_ID, BALANCE FROM ACCOUNTS')
    with open('C:/banking-mainframe-project/data/report.txt', 'w') as f:
        f.write('End of Day Report\n')
        total = 0
        for row in cursor.fetchall():
            f.write(f'Account: {row[0]} Bal: {row[1]}\n')
            total += row[1]
        f.write(f'Total: {total}\n')

# View transactions
def view_transactions():
    try:
        with open('C:/banking-mainframe-project/data/transactions.txt', 'r') as f:
            print("\nTransaction History:")
            print(f.read())
    except FileNotFoundError:
        print("\nNo transaction history yet.")

# COBOL integration with total interest
def calculate_interest():
    try:
        # Ensure account_data.ps is up-to-date with current balances
        with open('C:/banking-mainframe-project/scripts/bin/account_data.ps', 'w') as f:
            cursor.execute('SELECT ACCOUNT_ID, BALANCE FROM ACCOUNTS')
            for row in cursor.fetchall():
                f.write(f"{row[0]},{row[1]:.2f}\n")
        
        # Run COBOL program and capture output
        result = subprocess.run(["C:/banking-mainframe-project/scripts/bin/interest.exe"], capture_output=True, text=True)
        if result.returncode == 0:
            cobol_output = result.stdout
            print("COBOL Interest Calculation:\n" + cobol_output)
            interest_values = []
            for line in cobol_output.splitlines():
                if "Interest Calculated:" in line:
                    interest = float(line.split("Interest Calculated:")[1].strip())
                    interest_values.append(interest)
            total_interest = sum(interest_values) if interest_values else 0.0
            
            with open('C:/banking-mainframe-project/data/report.txt', 'a') as f:
                f.write("COBOL Interest Calculations:\n")
                f.write(cobol_output)
                f.write(f"\nTotal Interest Calculated: {total_interest:.2f}\n")
        else:
            print("COBOL execution failed:", result.stderr)
    except Exception as e:
        print("Error calling COBOL:", e)

# Main program flow
if __name__ == "__main__":
    calculate_interest()  # Run at startup for console output
    while True:
        view_transactions()
        acct_id = input('Enter Account ID (0 to exit): ')
        if acct_id == '0':
            break
        trans_type = input('Enter Type (D=Deposit, W=Withdrawal): ').upper()
        amount = float(input('Enter Amount: '))
        try:
            process_transaction(int(acct_id), trans_type, amount)
        except ValueError:
            print('Invalid input. Use numbers for ID and amount.')
    generate_report()
    calculate_interest()  # Write to report.txt after report
    conn.close()