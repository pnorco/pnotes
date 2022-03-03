module Server.Router where

import Prelude

import Data.Array (length, take)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Node.FS.Aff (exists, readFile) as FS
import HTTPure as HTTPure


router :: HTTPure.Request -> HTTPure.ResponseM
router { body, headers, method, path } = case method, path of
  
  HTTPure.Get, [ "ping" ] -> HTTPure.ok $ "pong"
  HTTPure.Get, [ ] -> serveFile' "text/html" "src/wwwroot/index.html"
  HTTPure.Get, [ "static", filename ] -> serveFile ("src/wwwroot/" <> filename)

  HTTPure.Get, _ 
    | startsWith path [ "../" ] -> HTTPure.unauthorized

  _, _ -> do
    log $ "Not found: " <> show path
    HTTPure.unauthorized

startsWith :: Array String -> Array String -> Boolean
startsWith s t = t == (take (length t) s)

serveFile' :: String -> String -> HTTPure.ResponseM
serveFile' contentType path = (liftAff $ FS.exists path) >>= case _ of
  false -> HTTPure.notFound
  true -> (liftAff $ FS.readFile path) >>= (HTTPure.ok' (HTTPure.headers [ "Content-Type" /\ contentType ]))

serveFile :: String -> HTTPure.ResponseM
serveFile path = (liftAff $ FS.exists path) >>= case _ of
  false -> HTTPure.notFound
  true -> (liftAff $ FS.readFile path) >>= (HTTPure.ok)
  