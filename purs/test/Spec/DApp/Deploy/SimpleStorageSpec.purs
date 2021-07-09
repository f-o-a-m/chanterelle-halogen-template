module Spec.DApp.Deploy.SimpleStorageSpec (simpleStorageSpec) where

import Prelude
import Chanterelle.Test (assertWeb3)
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Effect.Aff (Aff, Milliseconds(..), delay, joinFiber)
import Effect.Aff.AVar (put, empty, take)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Network.Ethereum.Core.BigNumber (divide)
import Network.Ethereum.Types (Address)
import Network.Ethereum.Web3 (BigNumber, ChainCursor(Latest), Change(..), EventAction(..), Provider, _from, _gas, _to, defaultTransactionOptions, embed, event, eventFilter, forkWeb3, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

simpleStorageSpec ::
  forall e.
  Monad e =>
  { accounts :: Array Address
  , provider :: Provider
  , simpleStorage ::
      { address :: Address
      , initialCount :: UIntN S256
      }
  } ->
  SpecT Aff Unit e Unit
simpleStorageSpec { provider, accounts, simpleStorage } = do
  let
    primaryAccount = unsafePartial fromJust (accounts !! 0)

    ssTxOpts =
      defaultTransactionOptions # _to ?~ simpleStorage.address
        # _from
        ?~ primaryAccount
  describe "Should test the deployment of SimpleStorage"
    $ do
        it "The initial count agrees with the deploy config" do
          c <- assertWeb3 provider $ SimpleStorage.count ssTxOpts Latest
          c `shouldEqual` Right simpleStorage.initialCount
        it "Can update the count and monitor for even numbers"
          $ do
              -- Define the filter and how the countMonitor processes events
              let
                countFilter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage.address

                -- countMonitor ::
                --   forall eff.
                --   AVar (UIntN S256) ->
                --   SimpleStorage.CountSet ->
                --   ReaderT Change (Web3 ( avar :: AVAR, console :: C.CONSOLE | eff )) EventAction
                countMonitor countVar = \(SimpleStorage.CountSet { newCount }) -> do
                  Change changeLog <- ask
                  liftAff $ Console.log $ "Received SetCount event for transaction: " <> show changeLog.transactionHash
                  if isEvenBN $ unUIntN newCount then
                    liftAff (put newCount countVar) *> pure TerminateEvent
                  else
                    pure ContinueEvent
              -- Run the count monitor asynchronously
              countFiber <-
                forkWeb3 provider
                  $ do
                      countVar <- liftAff empty
                      void $ event countFilter (countMonitor countVar)
                      liftAff $ take countVar
              -- Submit two transactions, the countMonitor should end only with the second one.
              let
                txOptions = ssTxOpts # _gas ?~ embed 100000

                newCount1 = unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed 1

                newCount2 = unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed 2
              assertWeb3 provider
                $ do
                    txHash1 <- SimpleStorage.setCount txOptions { _newCount: newCount1 }
                    liftAff do
                      Console.log $ "Submitted setCount transaction for newCount1: " <> show txHash1
                      delay $ Milliseconds 3000.0
                    txHash2 <- SimpleStorage.setCount txOptions { _newCount: newCount2 }
                    liftAff $ Console.log $ "Submitted setCount transaction for newCount2: " <> show txHash2
              -- Await until the asynchronous counMonitor process terminates, check the result
              eCount <- joinFiber countFiber
              case eCount of
                Left err -> fail $ "Failed to establish countMonitor: " <> show err
                Right c -> c `shouldEqual` newCount2

isEvenBN :: BigNumber -> Boolean
isEvenBN bn
  | (bn `divide` embed 2) * embed 2 == bn = true
  | otherwise = false
