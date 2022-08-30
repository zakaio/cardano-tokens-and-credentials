 # Token-Based credential.

## Association ot DID and address

```mermaid
sequenceDiagram
actor Customer
actor TCProxy
actor Blockchain
Customer -> Blockchain: (1) submitDid  (web UI ?)
TCProxy ->> Blockchain: (2)  mint DidToken  which should be submitted to blockchain.
TCProxy ->> Customer: (3)  send code to claim did
Customer ->  Blockchain: (4) claimDid
```

