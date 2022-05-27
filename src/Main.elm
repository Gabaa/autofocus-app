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
    , isSettingsOpen : Bool
    , settings : Settings
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks =
            Array.fromList
                [ initTask "Brush teeth"
                , initTask "Comb hair"
                , initTask "Take a shower"
                , initTask "Eat a grape"
                ]
      , currentPage = 0
      , tasksPerPage = 10
      , newTask = ""
      , isSettingsOpen = False
      , settings = initSettings
      }
    , Cmd.none
    )


type alias Task =
    { name : String, done : Bool }


initTask : String -> Task
initTask name =
    { name = name, done = False }


type alias Settings =
    { placeholderBool : Bool }


initSettings : Settings
initSettings =
    { placeholderBool = True }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


type Msg
    = AddTask
    | SetTaskDone Int Bool
    | UpdateNewTask String
    | SetCurrentPage Int
    | ToggleSettingsOpen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                AddTask ->
                    case String.trim model.newTask of
                        "" ->
                            model

                        _ ->
                            { model | tasks = Array.push (initTask model.newTask) model.tasks, newTask = "" }

                SetTaskDone index done ->
                    case Array.get index model.tasks of
                        Just task ->
                            { model | tasks = Array.set index { task | done = done } model.tasks }

                        Nothing ->
                            model

                UpdateNewTask text ->
                    { model | newTask = text }

                SetCurrentPage pageNum ->
                    { model | currentPage = pageNum }

                ToggleSettingsOpen ->
                    { model | isSettingsOpen = not model.isSettingsOpen }
    in
    ( newModel, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Autofocus"
    , body =
        [ layout []
            (row [ width fill ]
                [ column [ width (fillPortion 1) ] []
                , column [ padding 32, width (fillPortion 2), spacing 32 ] (viewHeader :: viewContent model)
                , column [ width (fillPortion 1) ] []
                ]
            )
        ]
    }


viewHeader : Element Msg
viewHeader =
    let
        settingsButton =
            customButton []
                { onPress = Just ToggleSettingsOpen
                , label = text "Settings"
                }
    in
    row [ width fill ]
        [ el [ width fill ] none
        , el [ centerX, Font.size 32 ] (text "Autofocus App")

        {- Use this weird nesting to ensure that header element (above) is positioned exactly in the middle -}
        , el [ width fill ] (el [ alignRight ] settingsButton)
        ]


viewContent : Model -> List (Element Msg)
viewContent model =
    if model.isSettingsOpen then
        []

    else
        [ viewPageControlRow model
        , viewPage model.tasks model.currentPage model.tasksPerPage
        ]


viewPageControlRow : Model -> Element Msg
viewPageControlRow model =
    row [ width fill, spaceEvenly ]
        [ pageButton "Prev" model (model.currentPage - 1)
        , row [ width (px 400), spacing 16 ]
            [ Input.text []
                { onChange = UpdateNewTask
                , text = model.newTask
                , placeholder = Just (Input.placeholder [] (text "New task"))
                , label = Input.labelHidden "New task"
                }
            , customButton []
                { label = text "Add"
                , onPress = Just AddTask
                }
            ]
        , pageButton "Next" model (model.currentPage + 1)
        ]


viewPage : Array Task -> Int -> Int -> Element Msg
viewPage tasks currentPage tasksPerPage =
    let
        offset =
            currentPage * tasksPerPage
    in
    el [ width fill ]
        (column [ width fill, spacing 8 ]
            (List.range 0 (tasksPerPage - 1)
                |> List.map (\index -> viewTask (Array.get (offset + index) tasks) index)
            )
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

        textColor =
            if checked then
                lightGrey

            else
                black
    in
    Input.checkbox [ padding 8, Border.rounded 8, Border.glow grey 0.5, spacing 8 ]
        { onChange = SetTaskDone index
        , icon = Input.defaultCheckbox
        , checked = checked
        , label = Input.labelRight [] (el [ Font.color textColor ] (text label))
        }


customButton : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
customButton attributes properties =
    Input.button
        (List.concat
            [ [ padding 8, Border.rounded 8, Border.glow grey 0.5 ]
            , attributes
            ]
        )
        properties


pageButton : String -> Model -> Int -> Element Msg
pageButton label model pageNum =
    let
        validPageNum =
            isPageNumberValid model pageNum

        onPress =
            if validPageNum then
                Just (SetCurrentPage pageNum)

            else
                Nothing

        buttonAttrs =
            if validPageNum then
                []

            else
                [ Font.color lightGrey ]
    in
    customButton buttonAttrs
        { label = text label
        , onPress = onPress
        }


isPageNumberValid : Model -> Int -> Bool
isPageNumberValid model pageNum =
    let
        numberOfTasks =
            Array.length model.tasks

        numberOfPages =
            ceiling (toFloat numberOfTasks / toFloat model.tasksPerPage)
    in
    0 <= pageNum && pageNum < numberOfPages



-- Colors


black : Color
black =
    rgb255 0 0 0


grey : Color
grey =
    rgb255 127 127 127


lightGrey : Color
lightGrey =
    rgb255 191 191 191
