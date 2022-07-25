 # Token-Based credential.

## Association ot DID and address

```mermaid
sequenceDiagram
actor Customer
actor TCProxy
actor TCAddress
actor CustomerNewAddress
Customer ->> TCProxy: (1) link my Did with address.
TCProxy ->> Customer: (2) Code which should be submitted to blockchain.
alt customer use payment from own tool
  Customer ->> TCAddress: (3.1.1) transaction, wich include code from customer address.
else
  TCProxy ->> Customer: (3.2.1) new random address with key
  activate CustomerNewAddress
  Customer ->> CustomerNewAddress: (3.2.2) any transaction
  CustomerNewAddress ->> TCAddress: (3.2.3) transaction, which include code from CustomerNewAddress
end
TCProxy ->> Customer: (4) credential wich link did and address.
```

