module App.Config where

import Prelude

import Contracts.CryptoKitties as CK
import Data.Either (Either)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, CallError, ChainCursor(..), Web3, _to, defaultTransactionOptions, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)

ckAddress :: Address
ckAddress = unsafePartial fromJust $
            mkAddress =<< mkHexString "c7af99fe5513eb6710e6d5f44f9989da40f27f26"


tokenContract :: forall eff. Web3 eff (Either CallError Address)
tokenContract = CK.nonFungibleContract (defaultTransactionOptions # _to ?~ ckAddress) Latest
