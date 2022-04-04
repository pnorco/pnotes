module Common.Helpers.AjaxHelper where

import Prelude

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Common.Helpers.JSON (JSON(..), readResponse)
import Common.Model (ApiError(..))
import Common.Model as M
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Generic (class Decode)
import Simple.JSON (class WriteForeign, writeJSON)

type Request a =
  { token :: Maybe String
  , url :: String
  , content :: a
  }

request :: ∀ a b. WriteForeign a => Decode b => Method.Method -> Request a -> Aff (M.Response b)
request method req = 
  liftAff $ 
    Affjax.request 
      (Affjax.defaultRequest 
        { method = Left method
        , headers = maybe [] (\t -> [RequestHeader.RequestHeader "auth.sig" t]) req.token
        , url = req.url
        , content = Just $ RequestBody.string $ writeJSON req.content
        , responseFormat = ResponseFormat.string
        }
      ) >>= case _ of

    Left error -> 
      pure $ M.ResponseError (ApiTransportError (Affjax.printError error))
    Right response -> pure case response.status of
      StatusCode 200 -> case readResponse (JSON response.body) of
        Just content -> content
        Nothing -> M.ResponseError (ApiError response.body)
      _ -> M.ResponseError (ApiError if response.body == "" then response.statusText else response.body)

post :: ∀ a b. WriteForeign a => Decode b => Request a -> Aff (M.Response b)
post = request Method.POST

get :: ∀ a b. WriteForeign a => Decode b => Request a -> Aff (M.Response b)
get = request Method.GET