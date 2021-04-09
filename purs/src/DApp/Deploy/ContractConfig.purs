module DApp.Deploy.ContractConfig
  ( simpleStorageConfig
  ) where

import Prelude
import Chanterelle.Internal.Types (ContractConfig)
import Contracts.SimpleStorage as SimpleStorage
import Network.Ethereum.Web3 (embed)
import Network.Ethereum.Web3.Solidity (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Partial.Unsafe
import Data.Maybe

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------
simpleStorageConfig :: ContractConfig ( _count :: UIntN S256 )
simpleStorageConfig =
  { filepath: "dapp/build/abis/SimpleStorage.json"
  , name: "SimpleStorage"
  , constructor: SimpleStorage.constructor
  , unvalidatedArgs: pure { _count: initialCount }
  }
  where
  initialCount = unsafePartial $ fromJust $ (uIntNFromBigNumber s256 $ embed 42)
