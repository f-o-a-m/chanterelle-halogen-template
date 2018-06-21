module App.Config where

import Prelude

import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)

srAddress :: Address
srAddress = unsafePartial fromJust $
            mkAddress =<< mkHexString "41a322b28d0ff354040e2cbc676f0320d8c8850d"

--tokenContract :: forall eff. Web3 eff (Either CallError Address)
--tokenContract = CK.nonFungibleContract (defaultTransactionOptions # _to ?~ ckAddress) Latest
