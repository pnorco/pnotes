module Common.Helpers.JSON where

import Prelude

import Common.Model as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, class Encode)
import Simple.JSON (class ReadForeign, readJSON_, writeJSON)

newtype JSON = JSON String

readResponse :: ∀ a. Decode a => JSON -> Maybe (M.Response a)
readResponse (JSON s) = readJSON_ s

writeResponse :: ∀ a. Encode a => M.Response a -> JSON
writeResponse response = writeJSON response # JSON

readRequest :: ∀ a. ReadForeign a => JSON -> Maybe a
readRequest (JSON s) = readJSON_ s

derive instance Newtype JSON _ 