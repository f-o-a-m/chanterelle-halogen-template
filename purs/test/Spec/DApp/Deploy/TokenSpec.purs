module Spec.DApp.Deploy.TokenSpec (tokenSpec) where

import Prelude
import Chanterelle.Test (TestConfig, assertWeb3)
import Contracts.NavelCoin as NavelCoin
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ChainCursor(Latest), _from, _gas, _to, defaultTransactionOptions, embed)
import DApp.Deploy.Script (NavelCoinReceipt)
import Network.Ethereum.Web3.Types (ETH)
import Partial.Unsafe (unsafePartialBecause)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tokenSpec
  :: forall e r.
     TestConfig ( navelCoin :: NavelCoinReceipt
                | r
                )
  -> Spec ( eth     :: ETH
          , avar    :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
tokenSpec testConfig@{provider, accounts, navelCoin} = do
  let primaryAccount = unsafePartialBecause "Accounts list has at least one account" $ fromJust (accounts !! 0)

  describe "Should test ERC20 token preliminaries" $ do

    it "Deploying address owns all the tokens" $ do
      let txOptions = defaultTransactionOptions # _from ?~ primaryAccount
                                                # _to   ?~ navelCoin.address
                                                # _gas  ?~ embed 4712388

      bal <- assertWeb3 provider $ NavelCoin.balanceOf txOptions Latest {_owner: primaryAccount}
      bal `shouldEqual` Right navelCoin.totalSupply
