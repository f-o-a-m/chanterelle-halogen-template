module App.Model where

import Network.Ethereum.Web3.Types (Address, HexString, BlockNumber)
import Network.Ethereum.Web3.Solidity (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)

type TransferId = HexString

type AssetTransfer =
  { to :: Address
  , from :: Address
  , tokenId :: UIntN S256
  , transactionHash :: HexString
  , blockNumber :: BlockNumber
  , transferId :: TransferId
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
