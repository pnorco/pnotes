module Server.Main where

import Prelude

import Effect.Class.Console (log)
import HTTPure (ServerM, serve, ok)

main :: ServerM
main = serve 8080 router $ log "Server now up on port 8080"
  where
    router _ = ok "hello world!"