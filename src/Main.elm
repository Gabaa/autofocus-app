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
                    -- TODO: consider updating model to be Array (Maybe Task)
                    --       so that we can write to objects at any index
                    --       OR just enforce that you can only write to the
                    --       index just after the last (then updating array
                    --       is trivial). Last approach is probably better.
                    model

        Remove i ->
            { model | items = removeArrayItem i model.items }

        Add ->
            { model | items = Array.push (newTask "") model.items }

        TaskUpdateDone i done ->
            case Array.get i model.items of
                Just task ->
                    { model | items = Array.set i { task | done = done } model.items }

                Nothing ->
                    model


removeArrayItem : Int -> Array a -> Array a
removeArrayItem index items =
    Array.append
        (Array.slice 0 index items)
        (Array.slice (index + 1) (Array.length items) items)



-- View


view : Model -> Html Msg
view model =
    div []
        [ h2 []
            [ text "Hello!" ]
        , div
            []
            [ viewTaskList model ]
        ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    let
        viewTask index =
            let
                task =
                    Array.get index model.items |> Maybe.withDefault (newTask "")
            in
            li
                []
                [ div []
                    [ input [ type_ "checkbox", checked task.done, onCheck (TaskUpdateDone index) ] []
                    , input [ placeholder "Task", value task.name, disabled task.done, onInput (Update index) ] []
                    , button [ onClick (Remove index) ] [ text "X" ]
                    ]
                ]
    in
    ul [] (List.range 0 25 |> List.map viewTask)
