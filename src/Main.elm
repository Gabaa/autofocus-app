module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import List exposing (drop, indexedMap, take)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { items : List String }


init : Model
init =
    { items =
        [ "Brush teeth"
        , "Comb hair"
        , "Take a shower"
        , "Eat a grape"
        ]
    }



-- Update


type Msg
    = Update Int String
    | Remove Int
    | Add


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update index item ->
            { model | items = take index model.items ++ (item :: drop (index + 1) model.items) }

        Remove i ->
            { model | items = take i model.items ++ drop (i + 1) model.items }

        Add ->
            { model | items = model.items ++ [ "" ] }



-- View


view : Model -> Html Msg
view model =
    let
        renderItem index item =
            li []
                [ div []
                    [ input [ placeholder "Task", value item, onInput (Update index) ] []
                    , button [ onClick (Remove index) ] [ text "Delete" ]
                    ]
                ]
    in
    div []
        [ h2 []
            [ text "Hello!" ]
        , div
            []
            [ ul []
                (indexedMap renderItem model.items
                    ++ [ li []
                            [ button [ onClick Add ] [ text "New" ] ]
                       ]
                )
            ]
        ]
