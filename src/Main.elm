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


type alias Model =
    { todos : TodoList
    , key : Nav.Key
    , route : Route
    , fieldText : String
    }


type TodoList
    = TodoList (List TodoRecord)


type alias TodoRecord =
    { id : Int
    , label : String
    , status : Status
    }


type Todo
    = Todo TodoRecord


type Status
    = Active
    | Completed


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    ( Model emptyTodoList key (parseRoute url) ""
    , Cmd.none
    )


emptyTodoList : TodoList
emptyTodoList =
    TodoList []


isEmpty : TodoList -> Bool
isEmpty (TodoList list) =
    List.isEmpty list


addTodo : String -> TodoList -> TodoList
addTodo label (TodoList todos) =
    let
        newId =
            List.length todos + 1

        newLabel =
            String.trim label
    in
    TodoList (todos ++ [ TodoRecord newId newLabel Active ])


removeTodo : Int -> TodoList -> TodoList
removeTodo id (TodoList todos) =
    TodoList (List.filter ((/=) id << .id) todos)


mapTodo : Int -> (TodoRecord -> TodoRecord) -> TodoList -> TodoList
mapTodo id action (TodoList todos) =
    TodoList <|
        List.map
            (\todo ->
                if todo.id /= id then
                    todo

                else
                    action todo
            )
            todos


completeTodo : Int -> TodoList -> TodoList
completeTodo id list =
    mapTodo id (\todo -> { todo | status = Completed }) list


unCompleteTodo : Int -> TodoList -> TodoList
unCompleteTodo id list =
    mapTodo id (\todo -> { todo | status = Active }) list


clearCompleted : TodoList -> TodoList
clearCompleted (TodoList list) =
    TodoList (List.filter ((/=) Completed << .status) list)


completeAll : TodoList -> TodoList
completeAll (TodoList list) =
    TodoList (List.map (\todo -> { todo | status = Completed }) list)


unCompleteAll : TodoList -> TodoList
unCompleteAll (TodoList list) =
    TodoList (List.map (\todo -> { todo | status = Active }) list)


toTodos : List TodoRecord -> List Todo
toTodos =
    List.map Todo


getAllTodos : TodoList -> List Todo
getAllTodos (TodoList list) =
    toTodos list


getActiveTodos : TodoList -> List Todo
getActiveTodos (TodoList list) =
    toTodos (List.filter ((==) Active << .status) list)


getCompletedTodos : TodoList -> List Todo
getCompletedTodos (TodoList list) =
    toTodos (List.filter ((==) Completed << .status) list)


isCompleted : Todo -> Bool
isCompleted (Todo { status }) =
    status == Completed


todoLabel : Todo -> String
todoLabel (Todo { label }) =
    label


todoId : Todo -> Int
todoId (Todo { id }) =
    id


toJson : TodoList -> Encode.Value
toJson (TodoList list) =
    Encode.list
        (\todo ->
            Encode.object
                [ ( "id", Encode.int todo.id )
                , ( "label", Encode.string todo.label )
                , ( "completed", Encode.bool (todo.status == Completed) )
                ]
        )
        list


fromJson : Decode.Decoder TodoList
fromJson =
    let
        buildTodo id label completed =
            if completed then
                TodoRecord id label Completed

            else
                TodoRecord id label Active
    in
    Decode.map3
        buildTodo
        (Decode.field "id" Decode.int)
        (Decode.field "label" Decode.string)
        (Decode.field "completed" Decode.bool)
        |> Decode.list
        |> Decode.map TodoList



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
    , Cmd.batch [ saveTodos (toJson newModel.todos), cmd ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTodo ->
            if String.isEmpty model.fieldText then
                ( model, Cmd.none )

            else
                ( { model
                    | todos = addTodo model.fieldText model.todos
                    , fieldText = ""
                  }
                , Cmd.none
                )

        CheckTodo id isChecked ->
            let
                action =
                    if isChecked then
                        completeTodo

                    else
                        unCompleteTodo
            in
            ( { model
                | todos = action id model.todos
              }
            , Cmd.none
            )

        DeleteTodo id ->
            ( { model
                | todos = removeTodo id model.todos
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
                        completeAll

                    else
                        unCompleteAll
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
                | todos = clearCompleted model.todos
              }
            , Cmd.none
            )

        ReceivedTodos value ->
            ( case Decode.decodeValue fromJson value of
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
    fragment <| Maybe.withDefault "/"


parseRoute : Url.Url -> Route
parseRoute url =
    toRoute <| parse fragmentParser url



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


viewTodos : TodoList -> Route -> Html Msg
viewTodos todos route =
    let
        allSelected =
            List.all isCompleted (getAllTodos todos)

        selectedTodos =
            case route of
                AllTodos ->
                    getAllTodos todos

                ActiveTodos ->
                    getActiveTodos todos

                CompletedTodos ->
                    getCompletedTodos todos
    in
    section
        [ class "main"
        , hidden (isEmpty todos)
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


viewTodo : Todo -> Html Msg
viewTodo todo =
    li
        [ classList [ ( "completed", isCompleted todo ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ type_ "checkbox"
                , class "toggle"
                , onCheck (CheckTodo (todoId todo))
                , checked False
                , attributeIf (isCompleted todo) (checked True)
                ]
                []
            , Html.label
                []
                [ text <| todoLabel todo ]
            , button
                [ class "destroy", onClick (DeleteTodo (todoId todo)) ]
                []
            ]
        ]


viewFooter : TodoList -> Route -> Html Msg
viewFooter todos route =
    let
        entriesCompleted =
            List.length (getCompletedTodos todos)

        entriesLeft =
            List.length (getActiveTodos todos)
    in
    footer
        [ class "footer"
        , hidden (isEmpty todos)
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
