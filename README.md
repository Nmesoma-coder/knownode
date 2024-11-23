# KnowNode - Decentralized Education Credentials

KnowNode is a decentralized education credentials system built on the Stacks blockchain using Clarity smart contracts. It enables peer-based skill verification and creates tamper-proof educational credentials through a transparent assessment process.

## Features

- **Decentralized Skill Verification**: Skills are verified through peer assessment rather than central authorities
- **Reputation System**: Users build reputation through participation and successful verifications
- **Transparent Assessment**: All skill verifications are recorded on-chain and publicly verifiable
- **Secure Credentials**: Credentials cannot be forged or tampered with once verified

## Smart Contract Architecture

### Core Components

1. **User Management**
   - User registration system
   - Reputation tracking
   - Skill portfolio management

2. **Skill Framework**
   - Skill definition and management
   - Required assessment thresholds
   - Skill categorization

3. **Assessment System**
   - Peer assessment submission
   - Score aggregation
   - Verification thresholds
   - Timestamp tracking

### Data Structures

- `users`: Maps user addresses to their profiles and credentials
- `skills`: Stores skill definitions and requirements
- `skill-assessments`: Tracks assessment data for each skill verification attempt

## How to Use

### For Users

1. Register on the platform:
```clarity
(contract-call? .knownode register-user)
```

2. Request assessment for a skill:
```clarity
(contract-call? .knownode request-assessment u1)  ;; u1 is the skill ID
```

### For Assessors

1. Submit an assessment:
```clarity
(contract-call? .knownode submit-assessment u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u80)
```

### For Administrators

1. Add a new skill to the platform:
```clarity
(contract-call? .knownode add-skill "Blockchain Development" "Ability to develop smart contracts" u3)
```

## Security Considerations

- Minimum of 3 assessors required for skill verification
- 70% approval threshold for skill verification
- Assessors cannot review their own submissions
- Scores must be between 0-100
- Only contract owner can add new skills

## Future Enhancements

1. Implementation of skill prerequisites
2. Advanced reputation scoring system
3. Integration with educational institutions
4. Assessment staking mechanism
5. Automated skill verification for certain technical skills

## Contributing

We welcome contributions to KnowNode! Please read our contributing guidelines before submitting pull requests.

