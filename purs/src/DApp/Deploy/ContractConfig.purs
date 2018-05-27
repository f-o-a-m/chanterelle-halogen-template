module DApp.Deploy.ContractConfig
  ( navelCoinConfig
  ) where

import Prelude
import Chanterelle.Internal.Types (ContractConfig)
import Chanterelle.Deploy ((??))
import Contracts.NavelCoin as NavelCoin
import Network.Ethereum.Core.BigNumber (parseBigNumber, hexadecimal)
import Network.Ethereum.Web3.Solidity (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)

--------------------------------------------------------------------------------
-- | FoamToken
--------------------------------------------------------------------------------

navelCoinConfig :: ContractConfig (totalSupply :: UIntN S256)
navelCoinConfig =
  { filepath: "abis/NavelCoin.json"
  , name: "NavelCoin"
  , constructor: NavelCoin.constructor
  , unvalidatedArgs: {totalSupply: _} <$> totalSupply
  }
  where
    totalSupply =
      let mSupply = parseBigNumber hexadecimal "fffffffffffffffffffffff" >>= uIntNFromBigNumber s256
      in mSupply ?? "Coudn't parse totalSupply."
