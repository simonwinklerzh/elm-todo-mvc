module Main exposing (main)

import Browser
import Html
import Html.Events exposing ( onInput, onSubmit )
import Html.Attributes exposing ( class, attribute, type_, value, placeholder )

-- Model

type alias Entry =
    { text : String
    , checked : Bool
    , id : Int
    }

type Filter
    = All
    | Active
    | Completed

type alias Model =
    { entries : List Entry
    , input : String
    , filter : Filter
    }

type Msg
    = Input String
    | Add
    | Mark
    | Remove
    | Filter

initModel : Model
initModel =
    { entries = []
    , input = ""
    , filter = All
    }

-- Update

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input new_todo_name ->
            { model | input = new_todo_name }
        Add ->
            model
        Mark ->
            model
        Remove ->
            model
        Filter ->
            model

--add : Model -> Model
--add model =
--    let 
--        new_entry =
--            { text = model.input, checked = false, id = List.length model.entries.length + 1 }
--    in
--    { model |  }

-- View

todos_header : String -> Html.Html Msg
todos_header input =
    Html.form
        [ onSubmit Add ]
        [ Html.input
          [ type_ "text"
          , value input
          , placeholder "What needs to be done?"
          , onInput Input
          ]
          []
        ]

todos_footer : Model -> Html.Html Msg
todos_footer model =
    Html.div
        [ class "todo__list-footer" ]
        [ if List.length model.entries > 0 then
            Html.p 
                [ class "todo__list-count" ]
                [ Html.text "N items left" ]
        else
            Html.text ""
        ]

todos_list_entry : Entry -> Html.Html Msg
todos_list_entry entry =
    Html.li
        [ class "todos__list-entry" 
        , attribute "data-id" (String.fromInt entry.id)
        ]
        [ Html.text entry.text ]

todos_list : List Entry -> Html.Html Msg
todos_list entries =
    if List.length entries > 0 then
        Html.ul
            [ class "todos__list" ]
            ( List.map todos_list_entry entries )
    else
        Html.text ""


view : Model -> Html.Html Msg
view model = Html.div
    [ class "todos" ]
    [ Html.h1
        [ class "todos__title" ]
        [ Html.text "Todos" ]
    , Html.div
        [ class "todos__list-wrapper" ]
        [ todos_header model.input
        , todos_list model.entries
        , todos_footer model
        ]
    ]

main : Program () Model Msg
main = 
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
