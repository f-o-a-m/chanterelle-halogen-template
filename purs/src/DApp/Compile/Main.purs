module DApp.Compile.Main (main) where

import Prelude

import Chanterelle (compileMain)
import Chanterelle.Genesis (runGenesisGenerator)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Network.Ethereum.Web3 (ETH)

main :: forall eff.
      Eff
        ( console :: CONSOLE
        , fs :: FS
        , process :: PROCESS
        , exception :: EXCEPTION
        , eth :: ETH
        | eff
        )
        Unit
main = do
  compileMain
  runGenesisGenerator "./cliquebait.json" "./cliquebait-generated.json"
