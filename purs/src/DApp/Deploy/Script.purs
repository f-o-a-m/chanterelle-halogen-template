module DApp.Deploy.Script (NavelCoinReceipt, deployScript) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import DApp.Deploy.ContractConfig (navelCoinConfig)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Network.Ethereum.Web3 (Address, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Solidity (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)

type NavelCoinReceipt =
  { totalSupply :: UIntN S256
  , address :: Address
  }

deployScript
  :: forall eff.
     DeployM eff {navelCoin :: NavelCoinReceipt}
deployScript = do
  (DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  navelDeployReceipt <- deployContract txOpts navelCoinConfig
  pure { navelCoin: { totalSupply: navelDeployReceipt.deployArgs.totalSupply
                    , address: navelDeployReceipt.deployAddress
                    }
       }
