{-# LANGUAGE BangPatterns #-}
module Network.ABCI.Server.App
  ( App(..)
  , runApp
  , appConduit
  , transformApp
  , withProto
  , Middleware
  , MessageType(..)
  , demoteRequestType
  , msgTypeKey
  , Request(..)
  , hashRequest
  , Response(..)
  , encodeLengthPrefix
  , requestDecoder
  , responseEncoder
  ) where

import           Control.Lens                         ((?~), (^.))
import           Control.Lens.Wrapped                 (Wrapped (..),
                                                       _Unwrapped')
import           Control.Monad                        (when)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), Value (..),
                                                       object, withObject, (.:),
                                                       (.=))
import           Data.Aeson.Types                     (Parser)
import qualified Data.ByteString                      as BS
import           Data.Function                        ((&))
import           Data.Kind                            (Type)
import qualified Data.ProtoLens                       as PL
import           Data.ProtoLens.Encoding.Bytes        (getVarInt, putVarInt,
                                                       runBuilder, runParser,
                                                       signedInt64ToWord, wordToSignedInt64)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           Network.ABCI.Server.App.DecodeError  (DecodeError)
import qualified Network.ABCI.Server.App.DecodeError  as DecodeError
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response

import           Crypto.Hash                          (hashWith)
import           Crypto.Hash.Algorithms               (SHA256 (..))
import           Data.ByteArray                       (convert)
import qualified Data.ByteArray.HexString             as Hex
import           Data.Int (Int64)
import           Data.Default.Class                   (Default (..))
import           Data.ProtoLens.Message               (Message (defMessage))
import           Data.ProtoLens.Prism                 (( # ))
import qualified Proto.Types                          as PT
import qualified Proto.Types_Fields                   as PT

import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

-- | Used to parametrize Request and Response types
data MessageType
  = MTEcho
  | MTFlush
  | MTInfo
  | MTSetOption
  | MTInitChain
  | MTQuery
  | MTBeginBlock
  | MTCheckTx
  | MTDeliverTx
  | MTEndBlock
  | MTCommit
  deriving (Eq, Ord, Enum)

msgTypeKey :: MessageType -> String
msgTypeKey m = case m of
  MTEcho       -> "echo"
  MTFlush      -> "flush"
  MTInfo       -> "info"
  MTSetOption  -> "setOption"
  MTInitChain  -> "initChain"
  MTQuery      -> "query"
  MTBeginBlock -> "beginBlock"
  MTCheckTx    -> "checkTx"
  MTDeliverTx  -> "deliverTx"
  MTEndBlock   -> "endBlock"
  MTCommit     -> "commit"

demoteRequestType :: forall (t :: MessageType). Request t -> MessageType
demoteRequestType req = case req of
  RequestEcho _       -> MTEcho
  RequestInfo _       -> MTInfo
  RequestSetOption _  -> MTSetOption
  RequestQuery _      -> MTQuery
  RequestCheckTx _    -> MTCheckTx
  RequestFlush _      -> MTFlush
  RequestInitChain _  -> MTInitChain
  RequestBeginBlock _ -> MTBeginBlock
  RequestDeliverTx _  -> MTDeliverTx
  RequestEndBlock _   -> MTEndBlock
  RequestCommit _     -> MTCommit

reqParseJSON :: forall t inner. FromJSON inner => MessageType -> (inner -> Request t) -> Value -> Parser (Request t)
reqParseJSON msgType ctr = withObject ("req:" <> expectedType) $ \v -> do
  actualType <- v .: "type"
  if actualType == expectedType
    then ctr <$> v .: "message"
    else fail $ "expected `type` to equal: " <> show expectedType <> ", but got: " <> show actualType
  where
    expectedType = msgTypeKey msgType

resParseJSON :: FromJSON inner => MessageType -> (inner -> Response t) -> Value -> Parser (Response t)
resParseJSON msgType ctr = withObject ("res:" <> expectedType) $ \v -> do
  actualType <- v .: "type"
  if actualType == "exception"
    then ResponseException <$> v .: "message"
  else if actualType == expectedType
    then ctr <$> v .: "message"
    else fail $ "expected `type` to equal: " <> show expectedType <> ", but got: " <> show actualType
  where
    expectedType = msgTypeKey msgType

reqResToJSON :: ToJSON inner => MessageType -> inner -> Value
reqResToJSON msgType message = reqResToJSON' (cs $ msgTypeKey msgType) message

reqResToJSON' :: ToJSON inner => Text -> inner -> Value
reqResToJSON' msgType message = object
  [ "type" .= String msgType, "message" .= toJSON message]

--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

-- Note: that there are 3 type of connection made by tendermint to the ABCI application:
-- * Info/Query Connection, sends only: Echo, Info and SetOption requests
-- * Mempool Connection, sends only: CheckTx and Flush requests
-- * Consensus Connection, InitChain,: BeginBlock, DeliverTx, EndBlock and  Commit requests
-- https://github.com/tendermint/tendermint/blob/v0.32.2/proxy/app_conn.go#L11-L41
data Request (m :: MessageType) :: Type where
  -- Info/Query Connection
  RequestEcho :: Request.Echo -> Request 'MTEcho
  RequestInfo :: Request.Info -> Request 'MTInfo
  RequestSetOption :: Request.SetOption -> Request 'MTSetOption
  RequestQuery :: Request.Query -> Request 'MTQuery
  -- Mempool Connection
  RequestCheckTx :: Request.CheckTx -> Request 'MTCheckTx
  RequestFlush :: Request.Flush -> Request 'MTFlush
  -- Consensus Connection
  RequestInitChain :: Request.InitChain -> Request 'MTInitChain
  RequestBeginBlock :: Request.BeginBlock -> Request 'MTBeginBlock
  RequestDeliverTx :: Request.DeliverTx -> Request 'MTDeliverTx
  RequestEndBlock :: Request.EndBlock -> Request 'MTEndBlock
  RequestCommit :: Request.Commit -> Request 'MTCommit

instance ToJSON (Request (t :: MessageType)) where
  toJSON (RequestEcho v)       = reqResToJSON MTEcho v
  toJSON (RequestInfo v)       = reqResToJSON MTInfo v
  toJSON (RequestSetOption v)  = reqResToJSON MTSetOption v
  toJSON (RequestQuery v)      = reqResToJSON MTQuery v
  toJSON (RequestCheckTx v)    = reqResToJSON MTCheckTx v
  toJSON (RequestFlush v)      = reqResToJSON MTFlush v
  toJSON (RequestInitChain v)  = reqResToJSON MTInitChain v
  toJSON (RequestBeginBlock v) = reqResToJSON MTBeginBlock v
  toJSON (RequestDeliverTx v)  = reqResToJSON MTDeliverTx v
  toJSON (RequestEndBlock v)   = reqResToJSON MTEndBlock v
  toJSON (RequestCommit v)     = reqResToJSON MTCommit v


instance FromJSON (Request 'MTEcho) where parseJSON = reqParseJSON MTEcho RequestEcho
instance FromJSON (Request 'MTInfo) where parseJSON = reqParseJSON MTInfo RequestInfo
instance FromJSON (Request 'MTSetOption) where parseJSON = reqParseJSON MTSetOption RequestSetOption
instance FromJSON (Request 'MTQuery) where parseJSON = reqParseJSON MTQuery RequestQuery
instance FromJSON (Request 'MTCheckTx) where parseJSON = reqParseJSON MTCheckTx RequestCheckTx
instance FromJSON (Request 'MTFlush) where parseJSON = reqParseJSON MTFlush RequestFlush
instance FromJSON (Request 'MTInitChain) where parseJSON = reqParseJSON MTInitChain RequestInitChain
instance FromJSON (Request 'MTBeginBlock) where parseJSON = reqParseJSON MTBeginBlock RequestBeginBlock
instance FromJSON (Request 'MTDeliverTx) where parseJSON = reqParseJSON MTDeliverTx RequestDeliverTx
instance FromJSON (Request 'MTEndBlock) where parseJSON = reqParseJSON MTEndBlock RequestEndBlock
instance FromJSON (Request 'MTCommit) where parseJSON = reqParseJSON MTCommit RequestCommit

hashRequest
  :: forall (t :: MessageType).
     Request t
  -> Hex.HexString
hashRequest req =
  let requestBytes :: BS.ByteString = case req of
        RequestEcho v       -> PL.encodeMessage $ v ^. _Wrapped'
        RequestFlush v      -> PL.encodeMessage $ v ^. _Wrapped'
        RequestInfo v       -> PL.encodeMessage $ v ^. _Wrapped'
        RequestSetOption v  -> PL.encodeMessage $ v ^. _Wrapped'
        RequestInitChain v  -> PL.encodeMessage $ v ^. _Wrapped'
        RequestQuery v      -> PL.encodeMessage $ v ^. _Wrapped'
        RequestBeginBlock v -> PL.encodeMessage $ v ^. _Wrapped'
        RequestCheckTx v    -> PL.encodeMessage $ v ^. _Wrapped'
        RequestDeliverTx v  -> PL.encodeMessage $ v ^. _Wrapped'
        RequestEndBlock v   -> PL.encodeMessage $ v ^. _Wrapped'
        RequestCommit v     -> PL.encodeMessage $ v ^. _Wrapped'
  in Hex.fromBytes @BS.ByteString . convert $ hashWith SHA256 requestBytes

withProto
  :: (forall (t :: MessageType). Request t -> a)
  -> PT.Request'Value
  -> a
withProto f value = case value of
  PT.Request'Echo echo -> f $ RequestEcho $ echo ^. _Unwrapped'
  PT.Request'Flush flush -> f $ RequestFlush $ flush ^. _Unwrapped'
  PT.Request'Info info -> f $ RequestInfo $ info ^. _Unwrapped'
  PT.Request'SetOption setOption -> f $ RequestSetOption $ setOption ^. _Unwrapped'
  PT.Request'InitChain initChain -> f $ RequestInitChain $ initChain ^. _Unwrapped'
  PT.Request'Query query -> f $ RequestQuery $ query ^. _Unwrapped'
  PT.Request'BeginBlock beginBlock -> f $ RequestBeginBlock $ beginBlock ^. _Unwrapped'
  PT.Request'CheckTx checkTx -> f $ RequestCheckTx $ checkTx ^. _Unwrapped'
  PT.Request'DeliverTx deliverTx -> f $ RequestDeliverTx $ deliverTx ^. _Unwrapped'
  PT.Request'EndBlock endBlock -> f $ RequestEndBlock $ endBlock ^. _Unwrapped'
  PT.Request'Commit commit -> f $ RequestCommit $ commit ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

data Response (m :: MessageType) :: Type where
  ResponseEcho :: Response.Echo -> Response 'MTEcho
  ResponseFlush :: Response.Flush -> Response 'MTFlush
  ResponseInfo :: Response.Info -> Response 'MTInfo
  ResponseSetOption :: Response.SetOption -> Response 'MTSetOption
  ResponseInitChain :: Response.InitChain -> Response 'MTInitChain
  ResponseQuery :: Response.Query -> Response 'MTQuery
  ResponseBeginBlock :: Response.BeginBlock -> Response 'MTBeginBlock
  ResponseCheckTx :: Response.CheckTx -> Response 'MTCheckTx
  ResponseDeliverTx :: Response.DeliverTx -> Response 'MTDeliverTx
  ResponseEndBlock :: Response.EndBlock -> Response 'MTEndBlock
  ResponseCommit :: Response.Commit -> Response 'MTCommit
  ResponseException :: forall (m :: MessageType) . Response.Exception -> Response m

instance ToJSON (Response (t :: MessageType)) where
  toJSON (ResponseEcho v)       = reqResToJSON MTEcho v
  toJSON (ResponseFlush v)      = reqResToJSON MTFlush v
  toJSON (ResponseInfo v)       = reqResToJSON MTInfo v
  toJSON (ResponseSetOption v)  = reqResToJSON MTSetOption v
  toJSON (ResponseInitChain v)  = reqResToJSON MTInitChain v
  toJSON (ResponseQuery v)      = reqResToJSON MTQuery v
  toJSON (ResponseBeginBlock v) = reqResToJSON MTBeginBlock v
  toJSON (ResponseCheckTx v)    = reqResToJSON MTCheckTx v
  toJSON (ResponseDeliverTx v)  = reqResToJSON MTDeliverTx v
  toJSON (ResponseEndBlock v)   = reqResToJSON MTEndBlock v
  toJSON (ResponseCommit v)     = reqResToJSON MTCommit v
  toJSON (ResponseException v)  = reqResToJSON' "exception" v

instance FromJSON (Response 'MTEcho) where parseJSON = resParseJSON MTEcho ResponseEcho
instance FromJSON (Response 'MTFlush) where parseJSON = resParseJSON MTFlush ResponseFlush
instance FromJSON (Response 'MTInfo) where parseJSON = resParseJSON MTInfo ResponseInfo
instance FromJSON (Response 'MTSetOption) where parseJSON = resParseJSON MTSetOption ResponseSetOption
instance FromJSON (Response 'MTInitChain) where parseJSON = resParseJSON MTInitChain ResponseInitChain
instance FromJSON (Response 'MTQuery) where parseJSON = resParseJSON MTQuery ResponseQuery
instance FromJSON (Response 'MTBeginBlock) where parseJSON = resParseJSON MTBeginBlock ResponseBeginBlock
instance FromJSON (Response 'MTCheckTx) where parseJSON = resParseJSON MTCheckTx ResponseCheckTx
instance FromJSON (Response 'MTDeliverTx) where parseJSON = resParseJSON MTDeliverTx ResponseDeliverTx
instance FromJSON (Response 'MTEndBlock) where parseJSON = resParseJSON MTEndBlock ResponseEndBlock
instance FromJSON (Response 'MTCommit) where parseJSON = resParseJSON MTCommit ResponseCommit

instance Default (Response 'MTEcho) where def = ResponseEcho def
instance Default (Response 'MTFlush) where def = ResponseFlush def
instance Default (Response 'MTInfo) where def = ResponseInfo def
instance Default (Response 'MTSetOption) where def = ResponseSetOption def
instance Default (Response 'MTInitChain) where def = ResponseInitChain def
instance Default (Response 'MTQuery) where def = ResponseQuery def
instance Default (Response 'MTBeginBlock) where def = ResponseBeginBlock def
instance Default (Response 'MTCheckTx) where def = ResponseCheckTx def
instance Default (Response 'MTDeliverTx) where def = ResponseDeliverTx def
instance Default (Response 'MTEndBlock) where def = ResponseEndBlock def
instance Default (Response 'MTCommit) where def = ResponseCommit def

-- | Translates type-safe 'Response' GADT to the unsafe
--   auto-generated 'Proto.Response'
toProto :: Response t -> PT.Response
toProto r = case r of
  ResponseEcho msg       -> wrap PT._Response'Echo msg
  ResponseFlush msg      -> wrap PT._Response'Flush msg
  ResponseInfo msg       -> wrap PT._Response'Info msg
  ResponseSetOption msg  -> wrap PT._Response'SetOption msg
  ResponseInitChain msg  -> wrap PT._Response'InitChain msg
  ResponseQuery msg      -> wrap PT._Response'Query msg
  ResponseBeginBlock msg -> wrap PT._Response'BeginBlock msg
  ResponseCheckTx msg    -> wrap PT._Response'CheckTx msg
  ResponseDeliverTx msg  -> wrap PT._Response'DeliverTx msg
  ResponseEndBlock msg   -> wrap PT._Response'EndBlock msg
  ResponseCommit msg     -> wrap PT._Response'Commit msg
  ResponseException msg  -> wrap PT._Response'Exception msg
  where
    wrap v msg = defMessage & PT.maybe'value ?~ v # (msg ^. _Wrapped')


-- | Application type that represents a well typed application, i.e. a
-- function from a typed `Request` to a typed `Response`.
newtype App m = App
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) }

-- | Middleware is a component that sits between the server and application.
-- It can do such tasks as logging or response caching. What follows is the general
-- definition of middleware, though a middleware author should feel free to modify this.
type Middleware m = App m -> App m

-- | Transform an application from running in a custom monad to running in `IO`.
transformApp :: (forall (t :: MessageType). m (Response t) -> g (Response t)) -> App m -> App g
transformApp nat (App f) = App $ nat . f

appConduit :: Monad m => App m -> ConduitT BS.ByteString BS.ByteString m ()
appConduit app = requestDecoder .| C.mapM (either (pure . onError) (runApp app)) .| responseEncoder

runApp :: forall m. Functor m => App m -> PT.Request'Value -> m PT.Response
runApp (App app) = withProto $ fmap toProto . app

onError :: DecodeError -> PT.Response
onError err = toProto $ ResponseException $ Response.Exception $ cs $ DecodeError.print err

-- | Encodes ByteStrings into varlength-prefixed ByteString
encodeLengthPrefix :: BS.ByteString -> BS.ByteString
encodeLengthPrefix bytes = header <> bytes
  where
    headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
    header = runBuilder $ putVarInt headerN

requestDecoder :: Monad m => ConduitT BS.ByteString (Either DecodeError PT.Request'Value) m ()
requestDecoder = do
  mbs <- C.await
  case mbs of
    Nothing -> return ()
    Just bs -> do
      -- actual payloads are prefixed with a variable size encoding of their length
      let lenres = runParser getVarInt bs
      case lenres of
        Left e -> C.yield (Left $ DecodeError.ProtoLensParseError bs e) >> requestDecoder
        Right len_ -> do
          let len = wordToSignedInt64 len_
              lenLength = BS.length $ runBuilder (putVarInt len_)
              bs' = BS.drop lenLength bs
          emsg <- parseN len bs'
          case emsg of
            Left e -> C.yield (Left e) -- no more bytes, no point in trying again
            Right msgBytes -> case PL.decodeMessage msgBytes of
              Left e -> C.yield (Left $ DecodeError.CanNotDecodeRequest msgBytes e) >> requestDecoder
              Right (req :: PT.Request)
                | Just v <- req ^. PT.maybe'value -> C.yield (Right v) >> requestDecoder
                | otherwise -> C.yield (Left $ DecodeError.NoValueInRequest msgBytes (req ^. PL.unknownFields))
                            >> requestDecoder

parseN :: Monad m => Int64 -> BS.ByteString -> ConduitT BS.ByteString o m (Either DecodeError BS.ByteString)
parseN len !acc
  | fromIntegral (BS.length acc) >= len = case BS.splitAt (fromIntegral len) acc of
      (bs, rest) -> when (BS.length rest /= 0) (C.leftover rest)
                 >> return (Right bs)
  | otherwise = do
      mbs <- C.await
      case mbs of
        Nothing -> return (Left DecodeError.NotEnoughBytes)
        Just bs -> parseN len (acc <> bs)

responseEncoder :: Monad m => ConduitT PT.Response BS.ByteString m ()
responseEncoder = C.map (encodeLengthPrefix . PL.encodeMessage)
