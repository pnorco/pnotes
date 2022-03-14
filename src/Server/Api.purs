module Server.Api where

import Prelude

import Common.Model (Note)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import HTTPure as HTTPure
import HTTPure.Body (RequestBody)
import SQLite3 (DBConnection)
import Server.Database as DB
import Simple.JSON as JSON

type Body = String

getNotes :: String -> HTTPure.ResponseM
getNotes token = do
  notes <- DB.getNotes token
  HTTPure.ok $ JSON.writeJSON {notes: notes}

saveNote :: String -> String -> HTTPure.ResponseM
saveNote token body = case JSON.readJSON body of
  Right (note :: Note) -> do
    notes <- DB.saveNote note
    HTTPure.ok "ok" -- $ JSON.writeJSON {notes: notes}
  Left err -> HTTPure.unauthorized

deleteNote :: String -> String -> HTTPure.ResponseM
deleteNote token createdAt = do
    notes <- DB.deleteNote createdAt
    HTTPure.ok "ok" -- $ JSON.writeJSON {notes: notes}