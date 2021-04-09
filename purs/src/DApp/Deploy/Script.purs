module DApp.Deploy.Script (SimpleStorageReceipt, deployScript) where

import Prelude
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import DApp.Deploy.ContractConfig (simpleStorageConfig)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Network.Ethereum.Web3 (Address, defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Web3.Solidity (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)

type SimpleStorageReceipt
  = { initialCount :: UIntN S256
    , address :: Address
    }

deployScript :: DeployM { simpleStorage :: SimpleStorageReceipt }
deployScript = do
  (DeployConfig { primaryAccount }) <- ask
  let
    bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"

    txOpts =
      defaultTransactionOptions # _from ?~ primaryAccount
        # _gas
        ?~ bigGasLimit
  simpleStorageReceipt <- deployContract txOpts simpleStorageConfig
  pure
    { simpleStorage:
        { initialCount: simpleStorageReceipt.deployArgs._count
        , address: simpleStorageReceipt.deployAddress
        }
    }
