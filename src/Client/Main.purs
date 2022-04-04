module Client.Main where

import Prelude

import Client.FFI as FFI
import Common.Helpers.AjaxHelper (get)
import Common.Model (Note)
import Common.Model as M
import Data.Array (cons, delete, findIndex, updateAt)
import Data.Const (Const)
import Data.DateTime (DateTime)
import Data.Either (fromRight)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Now (nowDateTime)
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (merge, never, throughAff)
import Spork.Transition (Transition, purely)

type Model = 
  { currentNote :: Maybe Note
  , notes :: Array Note
  , error :: Maybe String
  }

initModel :: Model
initModel = 
  { currentNote: Nothing
  , notes: []
  , error: Nothing
  }

data Action 
  = Initialize
  | Add
  | Added String
  | Focused String
  | Edit Note
  | Save
  | Delete Note
  | UpdateNoteTitle String
  | UpdateNoteContent String
  | NotesLoaded M.GetNotesResponse

update ∷ Model -> (Action -> Transition ActionAff Model Action)
update model = case _ of
  Initialize -> 
    { model: model, effects: App.lift ((GetNotes "token") NotesLoaded) }
  NotesLoaded reponse -> case reponse of
    M.ResponseError e -> purely model { error = Just (show e) }
    M.Response r ->
      purely model { notes = r }
  Add ->
    { model: model, effects: App.lift (AddAff Added) }
  Added date ->
    let current = fromMaybe {title:"", content:"", createdAt: "0"} model.currentNote
    in { model: model {currentNote = Just current {createdAt=date}}, effects: App.lift ((Focus "newNoteTitle") Focused) }
  Focused _ ->
    purely model
  Edit note ->
    { model: model {currentNote = Just note}, effects: App.lift ((Focus "newNoteTitle") Focused) }
  Save -> 
    let current = fromMaybe {title:"", content:"", createdAt: "0"} model.currentNote
        notes' = case findIndex (\a -> a.createdAt == current.createdAt) model.notes of
          Nothing -> cons current model.notes
          Just i -> fromMaybe model.notes (updateAt i current model.notes)
    in purely model { notes = notes', currentNote = Nothing}
  Delete note -> 
    purely model { notes = delete note model.notes }
  UpdateNoteTitle str -> 
    let note = fromMaybe {title:"", content:"", createdAt: "0"} model.currentNote
    in purely model { currentNote = Just note{ title=str } }
  UpdateNoteContent str ->
    let note = fromMaybe {title:"", content:"", createdAt: "0"} model.currentNote
    in purely model { currentNote = Just note{ content=str } }

data ActionAff next
  = AddAff (String -> next)
  | Focus String (String -> next)
  | GetNotes String (M.GetNotesResponse -> next)

displayDatetime :: DateTime -> String
displayDatetime d = 
    fromRight "no date" (formatDateTime "YYYY-MM-DD HH:mm:ss" d)

updateAff ∷ ActionAff ~> Aff
updateAff aff = case aff of
  AddAff next -> do
    createdAt <- liftEffect nowDateTime
    let createdStr = displayDatetime createdAt
    pure $ next createdStr
  Focus id next -> do
    _ <- liftEffect $ FFI.focus id
    pure $ next id
  GetNotes token next -> do
    notes <- get { url: "/api/notes", token: Just token, content: { } }
    pure $ next notes

render ∷ Model → Html Action
render model =
  H.div []
    [ case model.currentNote of
      Nothing -> 
        H.div 
          [ H.styles [ (H.Style "text-align" "center"), (H.Style "margin-bottom" "10px")]] 
          [ H.button [ H.onClick (H.always_ Add), H.id_ "addNote" ] [ H.text "Add Note" ]
          , H.button [ H.onClick (H.always_ Initialize), H.id_ "loadNotes" ] [ H.text "load notes" ]
          ]
      Just note -> 
        H.div [ H.classes ["note"] ] 
        [ H.input [ H.type_ H.InputText, H.id_ "newNoteTitle", H.placeholder "Title", H.classes ["title"], H.onValueInput (H.always UpdateNoteTitle), H.value note.title ]
        , H.textarea [ H.classes ["content"], H.placeholder "Content", H.onValueInput (H.always UpdateNoteContent), H.value note.content]
        , H.button
          [ H.onClick (H.always_ Save) ]
          [ H.text "save" ]
        ]
    , H.ul [H.classes ["notes"]] $ renderNote <$> model.notes
    , case model.error of
        Nothing -> H.text ""
        Just e -> 
          H.div 
          [ H.classes ["error-message", "notification", "is-warning"]] 
          [ H.button [H.classes ["delete"]] []
          , H.text e 
          ]
    ]

renderNote :: Note -> Html Action
renderNote note =
  H.li [H.classes ["note"], H.onClick (H.always_ (Edit note))] 
    [ H.i [H.classes ["mdi", "mdi-delete", "small", "delete"], H.onClick (H.always_ $ Delete note)] []
    , H.div [H.classes ["createdAt"]] [H.text note.createdAt]
    , H.div [H.classes ["title"]] [H.text note.title]
    , H.div [H.classes ["content"]] [H.text note.content]
    ]

handleErrors :: Error -> Effect Unit
handleErrors _ = pure unit

app ∷ App.App ActionAff (Const Void) Model Action
app = { update, render, subs: const mempty, init: purely initModel }

main ∷ Effect Unit
main = do
  let interpreter = throughAff updateAff handleErrors
  inst <- App.makeWithSelector (interpreter `merge` never) app "#app"
  inst.run