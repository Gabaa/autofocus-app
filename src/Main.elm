module Main exposing (main, viewTask)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Flags


type alias Flags =
    {}



-- Model


type alias Model =
    { tasks : Array Task
    , currentPage : Int
    , tasksPerPage : Int
    , newTask : String
    }


type alias Task =
    { name : String, done : Bool }


newTask : String -> Task
newTask name =
    { name = name, done = False }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks =
            Array.fromList
                [ newTask "Brush teeth"
                , newTask "Comb hair"
                , newTask "Take a shower"
                , newTask "Eat a grape"
                ]
      , currentPage = 0
      , tasksPerPage = 10
      , newTask = ""
      }
    , Cmd.none
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


type Msg
    = AddTask
    | SetTaskDone Int Bool
    | UpdateNewTask String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                AddTask ->
                    { model | tasks = Array.push (newTask model.newTask) model.tasks, newTask = "" }

                SetTaskDone index done ->
                    case Array.get index model.tasks of
                        Just task ->
                            { model | tasks = Array.set index { task | done = done } model.tasks }

                        Nothing ->
                            model

                UpdateNewTask text ->
                    { model | newTask = text }
    in
    ( newModel, Cmd.none )



-- View
{-
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

-}


view : Model -> Browser.Document Msg
view model =
    { title = "Autofocus"
    , body =
        [ layout []
            (row [ width fill ]
                [ column [ width (fillPortion 1) ] []
                , column [ padding 32, width (fillPortion 2), spacing 32 ]
                    [ el [ centerX, Font.size 32 ] (text "Autofocus App")
                    , row [ width fill, spaceEvenly ]
                        [ pageButton "Prev"
                        , row [ width (px 400), spacing 16 ]
                            [ Input.text []
                                { onChange = UpdateNewTask
                                , text = model.newTask
                                , placeholder = Just (Input.placeholder [] (text "New task"))
                                , label = Input.labelHidden "New task"
                                }
                            , customButton
                                { label = text "Add"
                                , onPress = Just AddTask
                                }
                            ]
                        , pageButton "Next"
                        ]
                    , row [] [ viewPage model.tasks model.currentPage model.tasksPerPage ]
                    ]
                , column [ width (fillPortion 1) ] []
                ]
            )
        ]
    }


viewPage : Array Task -> Int -> Int -> Element Msg
viewPage tasks currentPage tasksPerPage =
    let
        offset =
            currentPage * tasksPerPage
    in
    column [ spacing 8 ]
        (List.range 0 (tasksPerPage - 1)
            |> List.map (\index -> viewTask (Array.get (offset + index) tasks) index)
        )


viewTask : Maybe Task -> Int -> Element Msg
viewTask maybe_task index =
    let
        ( checked, label ) =
            case maybe_task of
                Just task ->
                    ( task.done, task.name )

                Nothing ->
                    ( False, " " )
    in
    Input.checkbox []
        { onChange = SetTaskDone index
        , icon = Input.defaultCheckbox
        , checked = checked
        , label = Input.labelRight [] (text label)
        }


black : Color
black =
    rgb255 0 0 0


customButton : { onPress : Maybe msg, label : Element msg } -> Element msg
customButton properties =
    Input.button
        [ padding 8, Border.rounded 8, Border.glow black 0.5 ]
        properties


pageButton : String -> Element Msg
pageButton label =
    customButton
        { label = text label
        , onPress = Nothing
        }
