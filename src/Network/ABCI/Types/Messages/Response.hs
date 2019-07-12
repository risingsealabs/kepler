module Network.ABCI.Types.Messages.Response where

import           Control.Lens                           (Iso', iso, traverse,
                                                         (&), (.~), (^.), (^..),
                                                         (^?), _Just)
import qualified Control.Lens                           as Lens
import           Data.ByteString                        (ByteString)
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word32, Word64)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams,
                                                         Evidence, Header,
                                                         KVPair, LastCommitInfo,
                                                         Proof, Timestamp,
                                                         ValidatorUpdate,
                                                         consensusParams,
                                                         evidence, header,
                                                         kVPair, lastCommitInfo,
                                                         proof, timestamp,
                                                         validatorUpdate)
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT

{-
       (ABCIApplication(..), BlockID(), BlockSizeParams(),
        ConsensusParams(), Evidence(), EvidenceParams(), Header(),
        LastCommitInfo(), PartSetHeader(), PubKey(), Request(),
        Request'Value(..), _Request'Echo, _Request'Flush, _Request'Info,
        _Request'SetOption, _Request'InitChain, _Request'Query,
        _Request'BeginBlock, _Request'CheckTx, _Request'DeliverTx,
        _Request'EndBlock, _Request'Commit, RequestBeginBlock(),
        RequestCheckTx(), RequestCommit(), RequestDeliverTx(),
        RequestEcho(), RequestEndBlock(), RequestFlush(), RequestInfo(),
        RequestInitChain(), RequestQuery(), RequestSetOption(), Response(),
        Response'Value(..), _Response'Exception, _Response'Echo,
        _Response'Flush, _Response'Info, _Response'SetOption,
        _Response'InitChain, _Response'Query, _Response'BeginBlock,
        _Response'CheckTx, _Response'DeliverTx, _Response'EndBlock,
        _Response'Commit, ResponseBeginBlock(), ResponseCheckTx(),
        ResponseCommit(), ResponseDeliverTx(), ResponseEcho(),
        ResponseEndBlock(), ResponseException(), ResponseFlush(),
        ResponseInfo(), ResponseInitChain(), ResponseQuery(),
        ResponseSetOption(), Validator(), ValidatorParams(),
        ValidatorUpdate(), Version(), VoteInfo())
-}

{-
data MessageType =
    Echo
  | Flus
  | Info
  | SetOption
  | InitChain
  | Query
  | BeginBlock
  | CheckTx
  | DeliverTx
  | EndBlock
  | Commit
-}


data BeginBlock =
  BeginBlock
    { beginBlockTags :: [KVPair]
    }

beginBlock :: Iso' BeginBlock PT.ResponseBeginBlock
beginBlock = iso to from
  where
    to BeginBlock{..} =
      defMessage
        & PT.tags .~ beginBlockTags ^.. traverse . kVPair
    from responseBeginBlock =
      BeginBlock
        { beginBlockTags = responseBeginBlock ^.. PT.tags . traverse . Lens.from kVPair
        }




data CheckTx =
  CheckTx
    { checkTxCode      :: Word32
    , checkTxData      :: ByteString
    , checkTxLog       :: Text
    , checkTxInfo      :: Text
    , checkTxGasWanted :: Int64
    , checkTxGasUsed   :: Int64
    , checkTxTags      :: [KVPair]
    , checkTxCodespace :: Text
    }

checkTx :: Iso' CheckTx PT.ResponseCheckTx
checkTx = iso to from
  where
    to CheckTx{..} =
      defMessage
        & PT.code .~ checkTxCode
        & PT.data' .~ checkTxData
        & PT.log .~ checkTxLog
        & PT.info .~ checkTxInfo
        & PT.gasWanted .~ checkTxGasWanted
        & PT.gasUsed .~ checkTxGasUsed
        & PT.tags .~ checkTxTags ^.. traverse . kVPair
        & PT.codespace .~ checkTxCodespace
    from responseCheckTx =
      CheckTx
        { checkTxCode = responseCheckTx ^. PT.code
        , checkTxData = responseCheckTx ^. PT.data'
        , checkTxLog = responseCheckTx ^. PT.log
        , checkTxInfo = responseCheckTx ^. PT.info
        , checkTxGasWanted = responseCheckTx ^. PT.gasWanted
        , checkTxGasUsed = responseCheckTx ^. PT.gasUsed
        , checkTxTags = responseCheckTx ^.. PT.tags . traverse . Lens.from kVPair
        , checkTxCodespace = responseCheckTx ^. PT.codespace
        }

data Commit =
  Commit
    { commitData :: ByteString
    }

commit :: Iso' Commit PT.ResponseCommit
commit = iso to from
  where
    to Commit{..} =
      defMessage
        & PT.data' .~ commitData
    from responseCommit =
      Commit
        { commitData = responseCommit ^. PT.data'
        }

data DeliverTx =
  DeliverTx
    { deliverTxCode      :: Word32
    , deliverTxData      :: ByteString
    , deliverTxLog       :: Text
    , deliverTxInfo      :: Text
    , deliverTxGasWanted :: Int64
    , deliverTxGasUsed   :: Int64
    , deliverTxTags      :: [KVPair]
    , deliverTxCodespace :: Text
    }

deliverTx :: Iso' DeliverTx PT.ResponseDeliverTx
deliverTx = iso to from
  where
    to DeliverTx{..} =
      defMessage
        & PT.code .~ deliverTxCode
        & PT.data' .~ deliverTxData
        & PT.log .~ deliverTxLog
        & PT.info .~ deliverTxInfo
        & PT.gasWanted .~ deliverTxGasWanted
        & PT.gasUsed .~ deliverTxGasUsed
        & PT.tags .~ deliverTxTags ^.. traverse . kVPair
        & PT.codespace .~ deliverTxCodespace
    from responseDeliverTx =
      DeliverTx
        { deliverTxCode = responseDeliverTx ^. PT.code
        , deliverTxData = responseDeliverTx ^. PT.data'
        , deliverTxLog = responseDeliverTx ^. PT.log
        , deliverTxInfo = responseDeliverTx ^. PT.info
        , deliverTxGasWanted = responseDeliverTx ^. PT.gasWanted
        , deliverTxGasUsed = responseDeliverTx ^. PT.gasUsed
        , deliverTxTags = responseDeliverTx ^.. PT.tags . traverse . Lens.from kVPair
        , deliverTxCodespace = responseDeliverTx ^. PT.codespace
        }

data Echo =
  Echo
    { echoMessage :: Text
    }

echo :: Iso' Echo PT.ResponseEcho
echo = iso to from
  where
    to Echo{..} =
      defMessage
        & PT.message .~ echoMessage
    from responseEcho =
      Echo
        { echoMessage = responseEcho ^. PT.message
        }

data EndBlock =
  EndBlock
    { endBlockValidatorUpdates      :: [ValidatorUpdate]
    , endBlockConsensusParamUpdates :: Maybe ConsensusParams
    , endBlockTags                  :: [KVPair]
    }

endBlock :: Iso' EndBlock PT.ResponseEndBlock
endBlock = iso to from
  where
    to EndBlock{..} =
      defMessage
        & PT.validatorUpdates .~ endBlockValidatorUpdates ^.. traverse . validatorUpdate
        & PT.maybe'consensusParamUpdates .~ endBlockConsensusParamUpdates ^? _Just . consensusParams
        & PT.tags .~ endBlockTags ^.. traverse . kVPair
    from responseEndBlock =
      EndBlock
        { endBlockValidatorUpdates = responseEndBlock ^.. PT.validatorUpdates . traverse . Lens.from validatorUpdate
        , endBlockConsensusParamUpdates = responseEndBlock ^? PT.maybe'consensusParamUpdates . _Just . Lens.from consensusParams
        , endBlockTags = responseEndBlock ^.. PT.tags . traverse . Lens.from kVPair
        }

data Exception =
  Exception
    { exceptionError :: Text
    }

exception :: Iso' Exception PT.ResponseException
exception = iso to from
  where
    to Exception{..} =
      defMessage
        & PT.error .~ exceptionError
    from responseException =
      Exception
        { exceptionError = responseException ^. PT.error
        }

data Flush =
  Flush

flush :: Iso' Flush PT.ResponseFlush
flush = iso to from
  where
    to Flush =
      defMessage
    from responseFlush =
      Flush

data Info =
  Info
    { infoData             :: Text
    , infoVersion          :: Text
    , infoAppVersion       :: Word64
    , infoLastBlockHeight  :: Int64
    , infoLastBlockAppHash :: ByteString
    }

info :: Iso' Info PT.ResponseInfo
info = iso to from
  where
    to Info{..} =
      defMessage
        & PT.data' .~ infoData
        & PT.version .~ infoVersion
        & PT.appVersion .~ infoAppVersion
        & PT.lastBlockHeight .~ infoLastBlockHeight
        & PT.lastBlockAppHash .~ infoLastBlockAppHash
    from responseInfo =
      Info
        { infoData = responseInfo ^. PT.data'
        , infoVersion = responseInfo ^. PT.version
        , infoAppVersion = responseInfo ^. PT.appVersion
        , infoLastBlockHeight = responseInfo ^. PT.lastBlockHeight
        , infoLastBlockAppHash = responseInfo ^. PT.lastBlockAppHash
        }

data InitChain =
  InitChain
    { initChainConsensusParams :: Maybe ConsensusParams
    , initChainValidators      :: [ValidatorUpdate]
    }

initChain :: Iso' InitChain PT.ResponseInitChain
initChain = iso to from
  where
    to InitChain{..} =
      defMessage
        & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . consensusParams
        & PT.validators .~ initChainValidators ^.. traverse . validatorUpdate
    from responseInitChain =
      InitChain
        { initChainConsensusParams = responseInitChain ^? PT.maybe'consensusParams . _Just . Lens.from consensusParams
        , initChainValidators = responseInitChain ^.. PT.validators . traverse . Lens.from validatorUpdate
        }

data Query =
  Query
    { queryCode      :: Word32
    , queryLog       :: Text
    , queryInfo      :: Text
    , queryIndex     :: Int64
    , queryKey       :: ByteString
    , queryValue     :: ByteString
    , queryProof     :: Maybe Proof
    , queryHeight    :: Int64
    , queryCodespace :: Text
    }

query :: Iso' Query PT.ResponseQuery
query = iso to from
  where
    to Query{..} =
      defMessage
        & PT.code .~ queryCode
        & PT.log .~ queryLog
        & PT.info .~ queryInfo
        & PT.index .~ queryIndex
        & PT.key .~ queryKey
        & PT.value .~ queryValue
        & PT.maybe'proof .~ queryProof ^? _Just . proof
        & PT.height .~ queryHeight
        & PT.codespace .~ queryCodespace
    from responseQuery =
      Query
        { queryCode = responseQuery ^. PT.code
        , queryLog = responseQuery ^. PT.log
        , queryInfo = responseQuery ^. PT.info
        , queryIndex = responseQuery ^. PT.index
        , queryKey = responseQuery ^. PT.key
        , queryValue = responseQuery ^. PT.value
        , queryProof = responseQuery ^? PT.maybe'proof . _Just . Lens.from proof
        , queryHeight = responseQuery ^. PT.height
        , queryCodespace = responseQuery ^. PT.codespace
        }

data SetOption =
  SetOption
    { setOptionCode :: Word32
    , setOptionLog  :: Text
    , setOptionInfo :: Text
    }

setOption :: Iso' SetOption PT.ResponseSetOption
setOption = iso to from
  where
    to SetOption{..} =
      defMessage
        & PT.code .~ setOptionCode
        & PT.log .~ setOptionLog
        & PT.info .~ setOptionInfo
    from responseSetOption =
      SetOption
        { setOptionCode = responseSetOption ^. PT.code
        , setOptionLog = responseSetOption ^. PT.log
        , setOptionInfo = responseSetOption ^. PT.info
        }
