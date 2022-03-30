module Test.Main where

import Prelude

import Data.Array (elem)
import Data.Array.NonEmpty (findIndex)
import Data.List (Pattern)
import Data.String as Array
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import HTTPure (found)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.Database as DB
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn, shouldSatisfy)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Unit (suite, test, timeout)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "DB" do
    it "insert" do
      let note = { title: "the test note", content: "bla bla bla", createdAt: "2022-03-19T10:20:30"}
      DB.saveNote note
      dbNotes <- DB.getNotes
      shouldReturn $ elem note dbNotes
      -- dbNotes `shouldContain` note