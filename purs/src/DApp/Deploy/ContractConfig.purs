module DApp.Deploy.ContractConfig
  ( simpleStorageConfig
  ) where

import Prelude

import Chanterelle.Deploy ((??))
import Chanterelle.Internal.Types (ContractConfig)
import Contracts.SimpleStorage as SimpleStorage
import Network.Ethereum.Web3 (embed)
import Network.Ethereum.Web3.Solidity (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig :: ContractConfig (_count :: UIntN S256)
simpleStorageConfig =
  { filepath: "dapp/build/abis/SimpleStorage.json"
  , name: "SimpleStorage"
  , constructor: SimpleStorage.constructor
  , unvalidatedArgs: {_count: _} <$> initialCount
  }
  where
    initialCount = (uIntNFromBigNumber s256 $ embed 42) ?? "Coudn't parse totalSupply."
