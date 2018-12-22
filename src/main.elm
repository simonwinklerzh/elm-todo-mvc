module Main exposing (main)

import Browser
import Html
import Html.Events
import Html.Attributes

type alias Entry =
    { text : Int
    , checked : Bool
    , id : Int
    }

type Filter
    = All
    | Active
    | Completed

type alias Model =
    { entries : List Entry
    , input : Maybe String
    , filter : Filter
    }

type Msg
    = Input
    | Add
    | Mark
    | Remove
    | Filter

main : Html.Html msg
main = Html.h1
    []
    [ Html.text "Hello World" ]
