# 📘 Tutorial: Building a DAO Governance Smart Contract on Cardano


## 🧭 Overview

In this tutorial, you’ll create and compile a **DAO Governance** validator script that governs community proposals through **on-chain voting**.
This script demonstrates **how decentralized governance works on Cardano**, where community members can **vote** (Yes/No) and **execute proposals** once a threshold is met.

You will:

1. Define the governance data structures
2. Write the validator logic
3. Compile the Plutus smart contract
4. Generate a `.plutus` file for deployment
5. Build and run it via Cabal

---

## ⚙️ 1. Smart Contract Structure

File: **`DaoGovernance.hs`**

### Language Extensions & Imports

These enable Plutus Template Haskell features and Ledger APIs:

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Prelude                          (IO, String, ($), (<>), print, putStrLn)
import           PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless, ($))
import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext(..), TxInfo(..), 
                                             Validator, mkValidatorScript,
                                             txInfoSignatories,
                                             unsafeFromBuiltinData, Value)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import           Cardano.Api                (writeFileTextEnvelope, PlutusScriptV2, 
                                             PlutusScript (..))
import           Cardano.Api.Shelley        (PlutusScript (..))
import           Codec.Serialise            (serialise)
```

---

## 🧾 2. DAO Data Types

A DAO typically handles **proposals** that members vote on.
We define a `Proposal` type to track the number of votes and threshold required to pass.

```haskell
data Proposal = Proposal
    { yesVotes      :: Integer
    , noVotes       :: Integer
    , threshold     :: Integer
    }
PlutusTx.unstableMakeIsData ''Proposal
```

This makes `Proposal` serializable to and from on-chain data.

### Governance Actions

Users can vote **Yes**, vote **No**, or **Execute** a passed proposal.

```haskell
data GovernanceAction = VoteYes | VoteNo | Execute
PlutusTx.unstableMakeIsData ''GovernanceAction
```

---

## 🧠 3. Governance Validator Logic

This is the heart of your DAO.

```haskell
{-# INLINABLE mkGovernanceValidator #-}
mkGovernanceValidator :: Proposal -> GovernanceAction -> ScriptContext -> Bool
mkGovernanceValidator datum action ctx =
    case action of
        VoteYes -> traceIfFalse "Voting threshold exceeded" (yesVotes datum + 1 <= threshold datum)
        VoteNo  -> traceIfFalse "Voting threshold exceeded" (noVotes datum + 1 <= threshold datum)
        Execute -> traceIfFalse "Proposal not approved" (yesVotes datum > noVotes datum)
```

### 🔍 How it works:

| Action      | Condition                                           | Result |
| ----------- | --------------------------------------------------- | ------ |
| **VoteYes** | Adds 1 to yes votes, ensures threshold not exceeded | Valid  |
| **VoteNo**  | Adds 1 to no votes, ensures threshold not exceeded  | Valid  |
| **Execute** | Ensures yes votes > no votes before execution       | Valid  |

If any condition fails, the transaction is **invalid** and reverts.

---

## 🔒 4. Plutus Wrapper

Every Plutus validator must accept these 3 arguments as raw `BuiltinData`:

```
BuiltinData -> BuiltinData -> BuiltinData -> ()
```

We wrap our governance logic like this:

```haskell
{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator datum redeemer context =
    let proposal   = unsafeFromBuiltinData datum        :: Proposal
        action     = unsafeFromBuiltinData redeemer     :: GovernanceAction
        ctx'       = unsafeFromBuiltinData context      :: ScriptContext
    in  if mkGovernanceValidator proposal action ctx'
            then ()
            else traceError "DAO Governance: action not allowed"
```

---

## 🏗️ 5. Compiling the Validator

We compile and serialize it to create the **on-chain Plutus script**:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
```

---

## 💾 6. Writing the `.plutus` File

```haskell
saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort . LBS.toStrict $ scriptSerialised
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope "dao-governance-validator.plutus" Nothing plutusScript
    case result of
        Left err -> print err
        Right () -> putStrLn "DAO Governance validator script written to dao-governance-validator.plutus"
```

---

## 🚀 7. Main Entry Point

```haskell
main :: IO ()
main = saveValidator
```

When run, this generates:

```
dao-governance-validator.plutus
```

---

## 📦 8. Cabal Configuration

Below is your relevant **Cabal section**:

```cabal
executable daogovernance-exe
  main-is:            DaoGovernance.hs
  hs-source-dirs:     tests
  build-depends:
      base >=4.14 && <5,
      plutus-ledger-api,
      plutus-tx,
      plutus-tx-plugin,
      bytestring,
      serialise,
      text,
      containers,
      cardano-api,
      cardano-ledger-core,
      cardano-ledger-shelley,
  default-language:   Haskell2010
```

📁 **Recommended Project Layout:**

```
wspace/
 ├── dao-governance-validator.plutus   (output)
 ├── tests/
 │    └── DaoGovernance.hs
 ├── wspace.cabal
 ├── cabal.project
 ├── LICENSE
 └── CHANGELOG.md
```

---

## 🧪 9. Build & Run Instructions

### Step 1: Update dependencies

```bash
cabal update
```

### Step 2: Build your executable

```bash
cabal build daogovernance-exe
```

### Step 3: Run it to generate the validator

```bash
cabal run daogovernance-exe
```

✅ Output:

```
DAO Governance validator script written to dao-governance-validator.plutus
```

---

## 🧩 10. Example Scenario

Let’s simulate how it works conceptually:

| Step | Action                                                              | Result                                   |
| ---- | ------------------------------------------------------------------- | ---------------------------------------- |
| 1    | Create proposal with `yesVotes = 0`, `noVotes = 0`, `threshold = 5` | Proposal Created                         |
| 2    | Members vote `VoteYes` repeatedly                                   | Each increases `yesVotes` by 1           |
| 3    | If `yesVotes` > `noVotes`                                           | Proposal can be **Executed**             |
| 4    | Execute proposal                                                    | Validator allows `Execute` action        |
| 5    | If `noVotes` ≥ `yesVotes`                                           | `Execute` fails: “Proposal not approved” |

---

## 🧱 11. Governance Logic Summary

| Component     | Description                             |
| ------------- | --------------------------------------- |
| **Datum**     | Stores proposal data (votes, threshold) |
| **Redeemer**  | Action taken (VoteYes, VoteNo, Execute) |
| **Validator** | Enforces correct governance flow        |
| **Condition** | Ensures valid voting/execution logic    |
| **Output**    | DAO proposal decision verified on-chain |

---

## 🌐 12. Next Steps

To expand this DAO contract:

* ✅ Add **proposal creation** logic
* ✅ Store **proposer’s address** for accountability
* ✅ Require **minimum token holding** to vote
* ✅ Add **execution scripts** for treasury transactions

You can also integrate this with:

* **Helios frontend** or **CIP-30 wallets** (Lace, Nami)
* A **DAO dashboard DApp** for community voting

---

## ✅ Summary

You’ve successfully:

* Created a **DAO governance validator**
* Defined proposal and voting logic
* Compiled and serialized a **Plutus V2** script
* Built and ran the contract using Cabal
* Learned how on-chain governance is structured in Cardano

