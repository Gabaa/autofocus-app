module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, br, button, div, h2, input, label, li, text, ul)
import Html.Attributes exposing (checked, disabled, for, id, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Task =
    { name : String, done : Bool }


newTask : String -> Task
newTask name =
    { name = name, done = False }


type alias Model =
    { pages : Array (Array Task)
    , newTask : String
    , currentPage : Int
    }


init : Model
init =
    { pages =
        Array.fromList
            [ Array.fromList
                [ newTask "Brush teeth"
                , newTask "Comb hair"
                , newTask "Take a shower"
                , newTask "Eat a grape"
                ]
            ]
    , newTask = ""
    , currentPage = 0
    }



-- Update


type Msg
    = Add String
    | TaskUpdateDone Int Bool
    | UpdateNewTask String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add name ->
            case Array.get model.currentPage model.pages of
                Just page ->
                    -- TODO: Handle the array overflowing
                    { model | pages = Array.set model.currentPage (Array.push (newTask name) page) model.pages }

                Nothing ->
                    model

        TaskUpdateDone index done ->
            case Array.get index model.items of
                Just task ->
                    { model | items = Array.set index { task | done = done } model.items }

                Nothing ->
                    model

        UpdateNewTask text ->
            { model | newTask = text }



-- View


tasksPerPage : Int
tasksPerPage =
    5


view : Model -> Html Msg
view model =
    div [ style "width" "600px", style "margin" "auto" ]
        [ h2 [] [ text "Hello!" ]
        , div []
            [ button [ disabled True ] [ text "Prev" ]
            , button [ disabled True ] [ text "Next" ]
            ]
        , br [] []
        , div []
            [ label [ for "new-task-field" ] [ text "New task:" ]
            , br [] []
            , input [ id "new-task-field", onInput UpdateNewTask ] []
            , button [ onClick (Add model.newTask) ] [ text "Add" ]
            ]
        , div
            []
            [ viewTaskList model ]
        ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    ul [] (List.range 0 (tasksPerPage - 1) |> List.map (viewTask model))


viewTask : Model -> Int -> Html Msg
viewTask model index =
    let
        maybeTask =
            Array.get index model.items

        done =
            case maybeTask of
                Just task ->
                    task.done

                Nothing ->
                    False

        finishable =
            maybeTask /= Nothing
    in
    li
        []
        [ div []
            [ input
                [ type_ "checkbox"
                , checked done
                , disabled (not finishable)
                , onCheck (TaskUpdateDone index)
                ]
                []
            , Maybe.map viewTaskText maybeTask |> Maybe.withDefault (text "")
            ]
        ]


viewTaskText : Task -> Html Msg
viewTaskText task =
    text task.name
