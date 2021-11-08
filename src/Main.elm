module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h2, input, li, text, ul)
import Html.Attributes exposing (checked, disabled, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Task =
    { done : Bool, name : String }


newTask : String -> Task
newTask name =
    { name = name, done = False }


type alias Model =
    { items : Array Task }


init : Model
init =
    { items =
        Array.fromList
            [ newTask "Brush teeth"
            , newTask "Comb hair"
            , newTask "Take a shower"
            , newTask "Eat a grape"
            ]
    }



-- Update


type Msg
    = Update Int String
    | Remove Int
    | Add
    | TaskUpdateDone Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update i item ->
            case Array.get i model.items of
                Just task ->
                    { model | items = Array.set i { task | name = item } model.items }

                Nothing ->
                    model

        Remove i ->
            { model | items = Array.append (Array.slice 0 i model.items) (Array.slice (i + 1) (Array.length model.items) model.items) }

        Add ->
            { model | items = Array.push (newTask "") model.items }

        TaskUpdateDone i done ->
            case Array.get i model.items of
                Just task ->
                    { model | items = Array.set i { task | done = done } model.items }

                Nothing ->
                    model



-- View


view : Model -> Html Msg
view model =
    let
        renderItem index item =
            li []
                [ div []
                    [ input [ type_ "checkbox", checked item.done, onCheck (TaskUpdateDone index) ] []
                    , input [ placeholder "Task", value item.name, disabled item.done, onInput (Update index) ] []
                    , button [ onClick (Remove index) ] [ text "X" ]
                    ]
                ]
    in
    div []
        [ h2 []
            [ text "Hello!" ]
        , div
            []
            [ ul []
                (List.append
                    (List.indexedMap renderItem (Array.toList model.items))
                    [ li []
                        [ button [ onClick Add ] [ text "New" ] ]
                    ]
                )
            ]
        ]
