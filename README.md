# 🛡️ Gig Worker Job Insurance Smart Contract 🛡️

This smart contract provides income protection insurance for freelancers and gig workers on the Stacks blockchain.

## 🌟 Features

- 📅 Daily or weekly insurance coverage options
- 💰 Flexible premium amounts with proportional coverage
- ⚡ Simple claim process after a waiting period
- 🔄 Policy management (purchase, claim, cancel)
- 💸 Automatic payout when eligible claims are made

## 📋 How It Works

1. **Purchase Insurance**: Freelancers can purchase insurance by paying a premium
2. **Coverage Period**: Choose between daily (24hr) or weekly (7 day) coverage
3. **Make Claims**: If work is lost, file a claim after the waiting period
4. **Receive Payout**: Get your coverage amount directly to your wallet

## 🚀 Usage Guide

### For Gig Workers

#### Purchase Insurance
```clarity
(contract-call? .gig-insurance purchase-insurance u1000000 u7)
```
- First parameter: Premium amount in microSTX (minimum 1 STX)
- Second parameter: Period type (u1 for daily, u7 for weekly)

#### Claim Insurance
```clarity
(contract-call? .gig-insurance claim-insurance u1)
```
- Parameter: Your policy ID

#### Cancel Policy
```clarity
(contract-call? .gig-insurance cancel-policy u1)
```
- Parameter: Your policy ID
- Note: You'll receive 50% of your premium as a refund

#### View Your Policies
```clarity
(contract-call? .gig-insurance get-user-policies tx-sender)
```

### For Contract Administrators

#### Set Fee Percentage
```clarity
(contract-call? .gig-insurance set-fee-percentage u5)
```
- Parameter: New fee percentage (0-20%)

#### Set Minimum Premium
```clarity
(contract-call? .gig-insurance set-min-premium u1000000)
```
- Parameter: Minimum premium amount in microSTX

#### Withdraw Fees
```clarity
(contract-call? .gig-insurance withdraw-fees)
```

## 📊 Coverage Calculation

- Daily coverage: Premium × 10
- Weekly coverage: Premium × 8

## ⚠️ Important Notes

- Claims can only be made after a 10-block waiting period
- Policies expire after their coverage period ends
- Each policy can only be claimed once
- Only the policy owner can claim or cancel a policy

## 🔒 Security

The contract includes various safety checks to ensure:
- Only authorized users can perform sensitive operations
- Funds are properly managed and accounted for
- Claims follow the established rules

## 🤝 Contributing

Feel free to submit issues or pull requests to improve this contract!

