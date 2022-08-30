# Token-Based credential.

## Association ot DID and address

```mermaid
sequenceDiagram
actor Customer

Customer ->> Blockchain: (1) submitDid  (web UI ?)
TCProxy ->> Blockchain: (2)  mint DidToken  which should be submitted to blockchain.
TCProxy ->> ProofSpace: (3) send code to claim did and addrress credential.
ProofSpace ->> Customer: (3.1) issue credential
Customer ->>  Blockchain: (4) claimDid
```

## Lock Value

```mermaid
sequenceDiagram
actor Customer
actor TCProxy
actor Blockchain
Customer ->> Blockchain: (1) lockValueToCred
Customer ->>  Blockchain: (2) claimLockedValue
```

### LockValue: web

```mermaid
sequenceDiagram
actor "Customer Browser"
actor "Customer ProofSpace App"
actor TCProxy
actor "ProofSpace Dashboard" 
actor Service
"Customer Browser" ->> TCProxy:  requestLockValueAddress
TCProxy ->> "Customer Browser": lockValueAddress
"Customer Browser" ->> Blockchain: (1.1) lockValueToCred
TCProxy ->> "ProofSpace Dashboard": (1.2) issue credential with txId and code
"ProofSpce Dashboard" ->> "Customer ProofSpace App": (2.2) issue credential with txId and code
alt
  "Customer ProofSpace App" ->> "ProofSpace Dashboard":  send cred to service
  "ProofSpace Dasboard" ->> Service:  credential
  Service ->> Blockchain: (2) claimLockedValue  (via TCProxy ?)
else
  "Customer ProofSpace App" ->> "Customer Browser": (receive credential to code)
  "Customer Browser" ->> Blockchain: (2) claimLockedValue
```



# Token-Based credential.
