module Server.Database where

import Prelude

import Common.Model (Note)
import Control.Monad.Reader (ask)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (unsafeToForeign)
import Node.FS.Aff as FS
import SQLite3 (DBConnection)
import SQLite3 as Sqlite
import Simple.JSON (class ReadForeign, read)

type InitDBType a = forall m. MonadAff m => m a

init :: InitDBType Sqlite.DBConnection
init = do
  let dbFilename = "notes.db"
  (liftAff $ FS.exists dbFilename) >>= case _ of
    false -> createDb dbFilename *> openDb dbFilename
    true -> openDb dbFilename

createDb :: String -> InitDBType Unit
createDb filename = do
  db <- liftAff $ Sqlite.newDB filename
  liftAff $ Sqlite.closeDB db
  run "CREATE TABLE IF NOT EXISTS notes( title text, content text, createdAt text PRIMARY KEY);" []

openDb :: String -> InitDBType Sqlite.DBConnection
openDb filename = liftAff $ Sqlite.newDB filename

query :: ∀ a. ReadForeign a => String -> Array String -> Aff (Array a)
query sql params = do
  db <- init
  liftAff (Sqlite.queryDB db sql (unsafeToForeign <$> params))
    <#> read
    >>> hush
    >>> fromMaybe []

run :: String -> Array String -> InitDBType Unit
run sql params = do
  db <- init
  liftAff $ void $ Sqlite.queryDB db sql (unsafeToForeign <$> params)

getNotes :: String -> Aff (Array Note)
getNotes str =
  query "SELECT * FROM notes" [ ]

saveNote :: Note -> Aff Unit
saveNote note = 
  run
    """
      INSERT INTO notes (title, content, createdAt) VALUES (@title, @content, @createdAt)
      ON CONFLICT(createdAt) DO UPDATE SET title=@title, content=@content WHERE createdAt=@createdAt
    """
    [ note.title, note.content, note.createdAt ]

deleteNote :: String -> Aff Unit
deleteNote createdAt =
  run "DELETE FROM notes WHERE createdAt=@createdAt" [ createdAt ]