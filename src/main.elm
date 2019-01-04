module Main exposing (main)

import Browser
import Html
import Html.Events exposing ( onInput, onSubmit, onClick )
import Html.Attributes exposing ( class, attribute, type_, value, placeholder, checked )

-- Model

type alias Id = Int

type alias Entry =
    { text : String
    , checked : Bool
    , id : Id
    }

type Filter
    = All
    | Active
    | Completed

type alias Model =
    { entries : List Entry
    , input : String
    , filter : Filter
    , current_id : Id
    , any_entry_is_checked : Bool
    }

type Msg
    = Input String
    | Add
    | Mark Entry
    | Remove Entry
    | Filter

initModel : Model
initModel =
    { entries = []
    , input = ""
    , filter = All
    , current_id = 0
    , any_entry_is_checked = False
    }

-- Update

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input new_todo_name ->
            { model | input = new_todo_name }
        Add ->
            add model
        Mark entry ->
            mark model entry
        Remove entry ->
            remove model entry
        Filter ->
            model

add : Model -> Model
add model =
    let 
        new_id = 
            model.current_id + 1
        new_entry =
            { text = model.input, checked = False, id = new_id }
    in
        { model 
            | entries = new_entry :: model.entries 
            , current_id = new_id
            , input = ""
        }

is_same_entry : Entry -> Entry -> Bool
is_same_entry a b = 
    a.id == b.id

is_not_same_entry : Entry -> Entry -> Bool
is_not_same_entry a b = not (is_same_entry a b)

remove : Model -> Entry -> Model
remove model entry =
    let 
        new_entries = List.filter (is_not_same_entry entry) model.entries
    in
        { model | entries = new_entries }

has_marked : List Entry -> Bool
has_marked entries =
    List.any (\entry -> entry.checked == True) entries

mark : Model -> Entry -> Model
mark model entry =
    let
        new_entries = List.map (\current_entry ->
                if is_same_entry entry current_entry then
                    { current_entry | checked = not current_entry.checked }
                else
                    current_entry
            ) model.entries
    in
        { model 
            | entries = new_entries 
            , any_entry_is_checked = has_marked new_entries
        }

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
    let
        list_length =
            String.fromInt (List.length model.entries) 
    in
            
    Html.div
        [ class "todo__list-footer" ]
        [ if List.length model.entries > 0 then
            Html.p 
                [ class "todo__list-count" ]
                [ Html.text list_length ]
        else
            Html.text ""
        , if model.any_entry_is_checked == True then
            Html.p
                []
                [ Html.text "Remove all checked" ]
        else 
            Html.text ""
        ]

todos_list_entry : Entry -> Html.Html Msg
todos_list_entry entry =
    Html.li
        [ class "todos__list-entry" 
        , attribute "data-id" (String.fromInt entry.id)
        ]
        [ Html.input
            [ type_ "checkbox"
            , onClick (Mark entry)
            , checked entry.checked
            ]
            []
        , Html.text entry.text
        , Html.button
            [ type_ "button"
            , onClick (Remove entry)
            ]
            [ Html.text "X" ]
        ]

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
