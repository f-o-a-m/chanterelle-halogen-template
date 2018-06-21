module App.Model where

import Prelude

import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Solidity (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (Address, BlockNumber(..), HexString, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)


type AssetTransfer =
  { to :: Address
  , from :: Address
  , tokenId :: UIntN S256
  , transactionHash :: HexString
  , blockNumber :: BlockNumber
  , transferId :: Int
  }

initialTransfer :: AssetTransfer
initialTransfer =
  let nullAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
      nullTokenId = unsafePartial fromJust $ uIntNFromBigNumber s256 zero
  in { to: nullAddress
     , from: nullAddress
     , tokenId: nullTokenId
     , transactionHash: mempty
     , blockNumber: BlockNumber zero
     , transferId: 0
     }

type List =
  { transfers :: Array Int
  , nextId :: Int
  }

initialList :: List
initialList =
  { transfers: []
  , nextId: 1
  }


type ImageState =
  { baseURL :: String
  , loadTryCount :: Int
  , loadStatus :: ImageLoadState
  }

data ImageLoadState = Loading | Loaded | Failed
data ImageAction = LoadFailed | LoadSucceeded | RetryLoading

initialImageState :: String -> ImageState
initialImageState baseURL =
  { baseURL
  , loadTryCount: 100
  , loadStatus: Loading
  }
