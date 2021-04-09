module DApp.Deploy.Main where

import Prelude
import Chanterelle.Deploy (deploy)
import DApp.Deploy.Script (deployScript)
import Data.Maybe (fromMaybe)
import Node.Process as NP
import Effect (Effect)

-- main :: Effect Unit
-- main = do
--   nodeUrl <- fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
--   -- deploy nodeUrl 60 deployScript
--   void deployScript
