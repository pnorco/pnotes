module Common.Model where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode, decode, defaultOptions, encode, genericDecode, genericEncode)
import Prelude (class Functor, class Show, Unit)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ApiError
  = ApiError String
  | ApiUnauthorized
  | ApiTransportError String
  | ApiGenericError
data Response a
  = ResponseError ApiError
  | Response a

type Note =
    { title :: String
    , content :: String
    , createdAt :: String
    }

type GetNotesResponse = Response (Array Note)
type SaveNoteResponse = Response String
type DeleteNoteResponse = Response String

derive instance Generic ApiError _
instance Encode ApiError where
  encode s = genericEncode defaultOptions s
instance Decode ApiError where
  decode s = genericDecode defaultOptions s
instance ReadForeign ApiError where
  readImpl = decode
instance WriteForeign ApiError where
  writeImpl = encode
instance Show ApiError where
  show = case _ of
    ApiError msg -> msg
    ApiUnauthorized -> "Invalid username or password"
    ApiTransportError msg -> msg
    ApiGenericError -> "An error has occurred"

derive instance Generic (Response a) _
derive instance Functor Response
instance Encode a => Encode (Response a) where
  encode s = genericEncode defaultOptions s
instance Decode a => Decode (Response a) where
  decode s = genericDecode defaultOptions s
instance Decode a => ReadForeign (Response a) where
  readImpl = decode
instance Encode a => WriteForeign (Response a) where
  writeImpl = encode 