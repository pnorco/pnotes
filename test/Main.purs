module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Server.Api as API
import Server.Database as DB
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "DB" do
    it "insert" do
      let note = { title: "the test note", content: "bla bla bla", createdAt: "2022-03-19T10:20:30"}
      DB.saveNote note
      dbNotes <- DB.getNotes ""
      dbNotes `shouldContain` note

    it "delete" do
      let note = { title: "the test note", content: "bla bla bla", createdAt: "2022-03-19T10:20:30"}
      DB.deleteNote "2022-03-19T10:20:30"
      dbNotes <- DB.getNotes ""
      dbNotes `shouldNotContain` note

  describe "API" do
    it "insert" do
      let noteStr = """
        { "title": "the test note"
        , "content": "bla bla bla"
        , "createdAt": "2022-03-19T10:20:30"
        }
      """
      let noteObj = { title: "the test note", content: "bla bla bla", createdAt: "2022-03-19T10:20:30"}
      _ <- API.saveNote "" noteStr
      dbNotes <- DB.getNotes ""
      dbNotes `shouldContain` noteObj

    it "delete" do
      let note = { title: "the test note", content: "bla bla bla", createdAt: "2022-03-19T10:20:30"}
      _ <- API.deleteNote "" "2022-03-19T10:20:30"
      dbNotes <- DB.getNotes ""
      dbNotes `shouldNotContain` note