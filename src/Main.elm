port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Hotkeys
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Extra exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import String
import TodoList as T
import Url
import Url.Parser exposing (..)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = updateWithStorage
        , subscriptions = \_ -> todoReader ReceivedTodos
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



---- PORT ----


port saveTodos : Encode.Value -> Cmd msg


port todoReader : (Encode.Value -> msg) -> Sub msg



---- MODEL ----


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    ( Model T.empty key (parseRoute url) ""
    , Cmd.none
    )


type alias Model =
    { todos : T.TodoList
    , key : Nav.Key
    , route : Route
    , fieldText : String
    }



---- UPDATE ----


type Msg
    = NewTodo
    | CheckTodo Int Bool
    | DeleteTodo Int
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | FieldChanged String
    | ClearCompleted
    | ToggleAll Bool
    | ReceivedTodos Encode.Value


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ saveTodos (T.toJson newModel.todos), cmd ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTodo ->
            if String.isEmpty model.fieldText then
                ( model, Cmd.none )

            else
                ( { model
                    | todos = T.add model.fieldText model.todos
                    , fieldText = ""
                  }
                , Cmd.none
                )

        CheckTodo id isChecked ->
            let
                action =
                    if isChecked then
                        T.complete

                    else
                        T.unComplete
            in
            ( { model
                | todos = action id model.todos
              }
            , Cmd.none
            )

        DeleteTodo id ->
            ( { model
                | todos = T.delete id model.todos
              }
            , Cmd.none
            )

        FieldChanged text ->
            ( { model
                | fieldText = text
              }
            , Cmd.none
            )

        UrlChanged url ->
            ( { model
                | route = parseRoute url
              }
            , Cmd.none
            )

        ToggleAll isChecked ->
            let
                action =
                    if isChecked then
                        T.completeAll

                    else
                        T.unCompleteAll
            in
            ( { model
                | todos = action model.todos
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model
                        | route = parseRoute url
                      }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ClearCompleted ->
            ( { model
                | todos = T.clearCompleted model.todos
              }
            , Cmd.none
            )

        ReceivedTodos value ->
            ( case Decode.decodeValue T.fromJson value of
                Ok list ->
                    { model | todos = list }

                Err _ ->
                    model
            , Cmd.none
            )



---- ROUTE ----


type Route
    = AllTodos
    | ActiveTodos
    | CompletedTodos


toRoute : Maybe String -> Route
toRoute string =
    case string of
        Just "/active" ->
            ActiveTodos

        Just "/completed" ->
            CompletedTodos

        _ ->
            AllTodos


fragmentParser : Parser (String -> a) a
fragmentParser =
    fragment (Maybe.withDefault "/")


parseRoute : Url.Url -> Route
parseRoute url =
    toRoute (parse fragmentParser url)



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm • TodoMVC"
    , body =
        [ div [ class "todomvc-wrapper" ]
            [ section
                [ class "todoapp" ]
                [ lazy viewHeader model.fieldText
                , lazy2 viewTodos model.todos model.route
                , lazy2 viewFooter model.todos model.route
                ]
            ]
        ]
    }


viewHeader : String -> Html Msg
viewHeader fieldText =
    header
        [ class "header" ]
        [ h1
            []
            [ text "Todos" ]
        , input
            [ class "new-todo"
            , autofocus True
            , placeholder "What needs to be done?"
            , name "newTodo"
            , value fieldText
            , onInput FieldChanged
            , Hotkeys.onKeyCode 13 NewTodo
            ]
            []
        ]


viewTodos : T.TodoList -> Route -> Html Msg
viewTodos todos route =
    let
        allSelected =
            List.all T.isCompleted (T.getAll todos)

        selectedTodos =
            case route of
                AllTodos ->
                    T.getAll todos

                ActiveTodos ->
                    T.getActive todos

                CompletedTodos ->
                    T.getCompleted todos
    in
    section
        [ class "main"
        , hidden (T.isEmpty todos)
        ]
        [ input
            [ id "toggle-all"
            , type_ "checkbox"
            , class "toggle-all"
            , name "toggle"
            , onCheck ToggleAll
            , checked allSelected
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , ul
            [ class "todo-list" ]
            (List.map viewTodo selectedTodos)
        ]


viewTodo : T.Todo -> Html Msg
viewTodo todo =
    li
        [ classList [ ( "completed", T.isCompleted todo ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ type_ "checkbox"
                , class "toggle"
                , onCheck (CheckTodo (T.id todo))
                , checked False
                , attributeIf (T.isCompleted todo) (checked True)
                ]
                []
            , Html.label
                []
                [ text <| T.label todo ]
            , button
                [ class "destroy", onClick (DeleteTodo (T.id todo)) ]
                []
            ]
        ]


viewFooter : T.TodoList -> Route -> Html Msg
viewFooter todos route =
    let
        entriesCompleted =
            List.length (T.getCompleted todos)

        entriesLeft =
            List.length (T.getActive todos)
    in
    footer
        [ class "footer"
        , hidden (T.isEmpty todos)
        ]
        [ lazy viewCount entriesLeft
        , lazy viewFilters route
        , lazy viewReset entriesCompleted
        ]


viewCount : Int -> Html Msg
viewCount count =
    let
        countLabel =
            if count == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong
            []
            [ text (String.fromInt count) ]
        , span
            []
            [ text countLabel ]
        , span
            []
            [ text " left" ]
        ]


viewReset : Int -> Html Msg
viewReset completed =
    button
        [ class "clear-completed"
        , hidden (completed == 0)
        , onClick ClearCompleted
        ]
        [ text ("Clear completed (" ++ String.fromInt completed ++ ")") ]


viewFilters : Route -> Html msg
viewFilters route =
    ul
        [ class "filters" ]
    <|
        List.map
            (viewLink route)
            [ Link "#/" "All" AllTodos
            , Link "#/active" "Active" ActiveTodos
            , Link "#/completed" "Completed" CompletedTodos
            ]


type alias Link =
    { path : String
    , label : String
    , route : Route
    }


viewLink : Route -> Link -> Html msg
viewLink currentRoute { path, label, route } =
    li
        []
        [ a
            [ href path
            , attributeIf (route == currentRoute) (class "selected")
            ]
            [ text label ]
        ]
