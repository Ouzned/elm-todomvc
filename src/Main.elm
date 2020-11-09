module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Hotkeys
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Extra exposing (..)
import Html.Events exposing (..)
import Html.Extra exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import List
import String
import Url
import Url.Parser exposing (..)



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


hasTodo : TodoList -> Bool
hasTodo (TodoList list) =
    list
        |> List.isEmpty
        |> not


addTodo : String -> TodoList -> TodoList
addTodo label (TodoList todos) =
    let
        newId =
            List.length todos + 1

        newLabel =
            String.trim label
    in
    TodoList <|
        todos
            ++ [ TodoRecord newId newLabel Active ]


removeTodo : Int -> TodoList -> TodoList
removeTodo id (TodoList todos) =
    TodoList <|
        List.filter (\todo -> todo.id /= id) todos


mapTodo : Int -> (TodoRecord -> TodoRecord) -> TodoList -> TodoList
mapTodo id f (TodoList todos) =
    TodoList <|
        List.map
            (\todo ->
                if todo.id /= id then
                    todo

                else
                    f todo
            )
            todos


completeTodo : Int -> TodoList -> TodoList
completeTodo id list =
    mapTodo id (\todo -> { todo | status = Completed }) list


unCompleteTodo : Int -> TodoList -> TodoList
unCompleteTodo id list =
    mapTodo id (\todo -> { todo | status = Active }) list


getAllTodos : TodoList -> List Todo
getAllTodos (TodoList list) =
    List.map (\todo -> Todo todo) list


getActiveTodos : TodoList -> List Todo
getActiveTodos list =
    List.filter (not << isCompleted) (getAllTodos list)


getCompletedTodos : TodoList -> List Todo
getCompletedTodos list =
    List.filter isCompleted (getAllTodos list)


clearCompleted : TodoList -> TodoList
clearCompleted (TodoList list) =
    TodoList <| List.filter (\todo -> todo.status /= Completed) list


completeAll : TodoList -> TodoList
completeAll (TodoList list) =
    TodoList <| List.map (\todo -> { todo | status = Completed }) list


unCompleteAll : TodoList -> TodoList
unCompleteAll (TodoList list) =
    TodoList <| List.map (\todo -> { todo | status = Active }) list


isCompleted : Todo -> Bool
isCompleted (Todo { status }) =
    case status of
        Active ->
            False

        Completed ->
            True


todoLabel : Todo -> String
todoLabel (Todo { label }) =
    label


todoId : Todo -> Int
todoId (Todo { id }) =
    id



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
            List.all isCompleted <| getAllTodos todos

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
        , hidden ((not << hasTodo) todos)
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
            todos
                |> getCompletedTodos
                |> List.length

        entriesLeft =
            todos
                |> getActiveTodos
                |> List.length
    in
    footer
        [ class "footer"
        , hidden ((not << hasTodo) todos)
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
