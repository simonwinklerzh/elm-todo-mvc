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
    , hidden : Bool
    , id : Id
    }

type Filter_type
    = All
    | Active
    | Completed

type alias Model =
    { entries : List Entry
    , input : String
    , filter : Filter_type
    , current_id : Id
    , any_entry_is_checked : Bool
    }

type Msg
    = Input String
    | Add
    | Mark Entry
    | Remove Entry
    | Remove_all_checked
    | Filter Filter_type

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
        Remove_all_checked ->
            { model | entries = remove_all_checked model.entries }
        Filter filter_type ->
            { model | filter = filter_type }

add : Model -> Model
add model =
    let 
        new_id = 
            model.current_id + 1
        new_entry =
            { text = model.input, checked = False, id = new_id, hidden = False }
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

remove_all_checked : List Entry -> List Entry
remove_all_checked entries =
    List.filter is_not_marked entries

remove_all_unchecked : List Entry -> List Entry
remove_all_unchecked entries =
    List.filter is_marked entries

is_marked : Entry -> Bool
is_marked entry =
    entry.checked == True

is_not_marked : Entry -> Bool
is_not_marked entry =
    entry.checked == False

has_marked : List Entry -> Bool
has_marked entries =
    List.any is_marked entries

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
        [ onSubmit Add 
        , class "todos__input-form"
        ]
        [ Html.input
          [ type_ "text"
          , class "todos__input-field"
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
            List.length model.entries
        length_display_string =
            if list_length == 1 then
                "1 item left"
            else
                (String.fromInt list_length) ++ " items left"
    in
            
    Html.div
        [ class "todo__list-footer" ]
        [ if List.length model.entries > 0 then
            Html.p 
                [ class "todo__list-count" ]
                [ Html.text length_display_string ]
        else
            Html.text ""
        , if List.length model.entries > 0 then
            Html.div
                [ class "todo__filter-container" ]
                [ Html.p
                    [ class (if model.filter == All then 
                        "todo__filter todo__filter--active" 
                    else 
                        "todo__filter"
                    )
                    , onClick (Filter All)
                    ]
                    [ Html.text "All" ]
                , Html.p
                    [ class (if model.filter == Active then
                        "todo__filter todo__filter--active"
                    else
                        "todo__filter"
                    )
                    , onClick (Filter Active)
                    ]
                    [ Html.text "Active" ]
                , Html.p
                    [ class (if model.filter == Completed then
                        "todo__filter todo__filter--active"
                    else
                        "todo__filter"
                    )
                    , onClick (Filter Completed)
                    ]
                    [ Html.text "Completed" ]
                ]
        else
            Html.text ""
        , Html.p
            [ class (if model.any_entry_is_checked then
                "todo__remove-all-checked"
            else
                "todo__remove-all-checked todo__remove-all-checked--disabled"
            )
            , onClick Remove_all_checked
            ]
            [ Html.text "Remove all checked" ]

        ]

todos_list_entry : Entry -> Html.Html Msg
todos_list_entry entry =
    if not entry.hidden == True then
        Html.li
            [ class "todos__list-entry" 
            , attribute "data-id" (String.fromInt entry.id)
            ]
            [ Html.input
                [ type_ "checkbox"
                , class "todos__list-entry-checkbox"
                , onClick (Mark entry)
                , checked entry.checked
                ]
                []
            , Html.text entry.text
            , Html.button
                [ type_ "button"
                , class "todos__list-entry-remove-button"
                , onClick (Remove entry)
                ]
                [ Html.text "X" ]
            ]
    else
        Html.text ""

todos_list : List Entry -> Html.Html Msg
todos_list entries =            
    if List.length entries > 0 then
        Html.ul
            [ class "todos__list" ]
            ( List.map todos_list_entry entries )
    else
        Html.text ""


view : Model -> Html.Html Msg
view model = 
    let
        filtered_entries =
            case model.filter of
                All ->
                    model.entries
                Active ->
                    remove_all_checked model.entries
                Completed ->
                    remove_all_unchecked model.entries
    in
        Html.div
            [ class "todos" ]
            [ Html.h1
                [ class "todos__title" ]
                [ Html.text "Todos" ]
            , Html.div
                [ class "todos__list-wrapper" ]
                [ todos_header model.input
                , todos_list filtered_entries
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
