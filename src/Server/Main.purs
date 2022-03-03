module Server.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure as HTTPure

import Server.Router (router)

main :: Effect Unit 
main = launchAff_ do
  let options = {hostname: "localhost", port: 8080, backlog: Nothing}
  liftEffect 
    $ HTTPure.serve' options router
    $ log ("Server now up @ " <> options.hostname <> ":" <> show options.port)