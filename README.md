# Decentralized Salary Streaming 
Stream crypto salaries by the second using Clarity smart contracts - no intermediaries needed!

## 🚀 Features

- Create salary streams with customizable duration
- Real-time streaming of payments
- Secure withdrawals for employees
- Employer deposit management
- Automatic stream calculations

## 📝 Contract Functions

### For Employers

- `deposit()`: Add funds to your employer balance
- `create-stream`: Start a new salary stream for an employee

### For Employees

- `withdraw`: Withdraw available streamed funds
- `calculate-streamed-amount`: Check available amount to withdraw

### Read-Only Functions

- `get-stream`: Get stream details
- `get-employer-balance`: Check employer's deposited balance

## 🔧 Usage

1. Employer deposits STX using `deposit()`
2. Employer creates stream with `create-stream`
3. Employee withdraws available funds using `withdraw`

## ⚡ Quick Start

```bash
clarinet contract call --contract-name decentralized-salary-streaming --function-name deposit
```

## 🔒 Security

- Only stream owners can withdraw funds
- Automatic balance checks
- Real-time payment calculations
```
