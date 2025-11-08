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


------------------------------------------------------------------------------------------
-- | DAO Governance Datum and Redeemer
------------------------------------------------------------------------------------------

-- Proposal state: store vote counts and voting threshold
data Proposal = Proposal
    { yesVotes      :: Integer
    , noVotes       :: Integer
    , threshold     :: Integer
    }
PlutusTx.unstableMakeIsData ''Proposal

-- Action on proposal: vote Yes/No or execute
data GovernanceAction = VoteYes | VoteNo | Execute
PlutusTx.unstableMakeIsData ''GovernanceAction

------------------------------------------------------------------------------------------
-- | Governance Validator Logic
------------------------------------------------------------------------------------------

{-# INLINABLE mkGovernanceValidator #-}
mkGovernanceValidator :: Proposal -> GovernanceAction -> ScriptContext -> Bool
mkGovernanceValidator datum action ctx =
    case action of
        VoteYes -> traceIfFalse "Voting threshold exceeded" (yesVotes datum + 1 <= threshold datum)
        VoteNo  -> traceIfFalse "Voting threshold exceeded" (noVotes datum + 1 <= threshold datum)
        Execute -> traceIfFalse "Proposal not approved" (yesVotes datum > noVotes datum)

------------------------------------------------------------------------------------------
-- | Wrap for Plutus
------------------------------------------------------------------------------------------

{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator datum redeemer context =
    let proposal   = unsafeFromBuiltinData datum        :: Proposal
        action     = unsafeFromBuiltinData redeemer     :: GovernanceAction
        ctx'       = unsafeFromBuiltinData context      :: ScriptContext
    in  if mkGovernanceValidator proposal action ctx'
            then ()
            else traceError "DAO Governance: action not allowed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

------------------------------------------------------------------------------------------
-- | Write compiled validator to file
------------------------------------------------------------------------------------------

saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort . LBS.toStrict $ scriptSerialised
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope "dao-governance-validator.plutus" Nothing plutusScript
    case result of
        Left err -> print err
        Right () -> putStrLn "DAO Governance validator script written to dao-governance-validator.plutus"

------------------------------------------------------------------------------------------
-- | Main entry point
------------------------------------------------------------------------------------------

main :: IO ()
main = saveValidator
