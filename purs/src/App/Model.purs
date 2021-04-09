module App.Model where

import Network.Ethereum.Web3.Solidity (UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Network.Ethereum.Web3.Types (Address, BlockNumber, HexString)

type AssetTransfer =
  { to :: Address
  , from :: Address
  , tokenId :: UIntN S256
  , transactionHash :: HexString
  , blockNumber :: BlockNumber
  , imageURL :: String
  }

type SRList =
  { transfers :: Array { transferId :: Int, transfer :: AssetTransfer }
  , nextId :: Int
  }

initialList :: SRList
initialList =
  { transfers: [] 
  , nextId: 1
  }

type Image =
  { baseURL :: String
  , loadTryCount :: Int
  , loadStatus :: ImageLoadState
  }

data ImageLoadState = Loading | Loaded | Failed

initialImage :: String -> Image
initialImage baseURL =
  { baseURL
  , loadTryCount: 100
  , loadStatus: Loading
  }
