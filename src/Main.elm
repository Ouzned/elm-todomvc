module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Hotkeys exposing (onKeyCode)
import Html exposing (Html, a, button, div, footer, h1, header, input, li, section, span, strong, text, ul)
import Html.Attributes exposing (autofocus, checked, class, href, name, placeholder, type_, value)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra exposing (viewIf)
import List exposing (length)
import String exposing (fromInt, isEmpty, trim)
import Url
import Url.Parser exposing (Parser, fragment, map, parse)



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
    ( Model emptyTodoList key (parseRoute url) "", Cmd.none )


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
    List.map
        (\todo ->
            if todo.id /= id then
                todo

            else
                f todo
        )
        todos
        |> TodoList


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
    List.filter (isCompleted >> not) (getAllTodos list)


getCompletedTodos : TodoList -> List Todo
getCompletedTodos list =
    List.filter isCompleted (getAllTodos list)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTodo ->
            if isEmpty model.fieldText then
                ( model, Cmd.none )

            else
                ( { model
                    | todos = addTodo model.fieldText model.todos
                    , fieldText = ""
                  }
                , Cmd.none
                )

        CheckTodo id isChecked ->
            ( if isChecked then
                { model | todos = completeTodo id model.todos }

              else
                { model | todos = unCompleteTodo id model.todos }
            , Cmd.none
            )

        DeleteTodo id ->
            ( { model | todos = removeTodo id model.todos }, Cmd.none )

        FieldChanged text ->
            ( { model | fieldText = text }, Cmd.none )

        UrlChanged url ->
            ( { model | route = parseRoute url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | route = parseRoute url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )



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


viewIfHasTodo : TodoList -> Html Msg -> Html Msg
viewIfHasTodo list msg =
    viewIf (hasTodo list) msg


view : Model -> Browser.Document Msg
view model =
    { title = "TodoMVC"
    , body =
        [ div [ class "todomvc-wrapper" ]
            [ section
                [ class "todoapp" ]
                [ viewHeader model.fieldText
                , viewIfHasTodo model.todos (viewTodos model)
                , viewIfHasTodo model.todos (viewFooter model)
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
            , onKeyCode 13 NewTodo
            ]
            []
        ]


viewTodos : Model -> Html Msg
viewTodos { todos, route } =
    let
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
        [ class "main" ]
        [ ul
            [ class "todo-list" ]
            (List.map viewTodo selectedTodos)
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    let
        id =
            todoId todo

        label =
            todoLabel todo
    in
    li [ attributeIf (isCompleted todo) (class "completed") ]
        [ div [ class "view" ]
            [ input
                [ type_ "checkbox"
                , class "toggle"
                , onCheck (CheckTodo id)
                , checked False
                , attributeIf (isCompleted todo) (checked True)
                ]
                []
            , Html.label [] [ text label ]
            , button [ class "destroy", onClick (DeleteTodo id) ] []
            ]
        ]


viewFooter : Model -> Html msg
viewFooter { todos, route } =
    footer
        [ class "footer" ]
        [ viewCount todos
        , viewFilters route
        ]


viewCount : TodoList -> Html msg
viewCount list =
    let
        count =
            list
                |> getActiveTodos
                |> length

        countLabel =
            if count > 1 then
                " items"

            else
                " item"
    in
    span
        [ class "todo-count" ]
        [ strong
            []
            [ text <| String.fromInt count ]
        , span
            []
            [ text countLabel ]
        , span
            []
            [ text " left" ]
        ]


viewFilters : Route -> Html msg
viewFilters route =
    ul
        [ class "filters" ]
        [ viewLink "#/" "All" <| route == AllTodos
        , viewLink "#/active" "Active" <| route == ActiveTodos
        , viewLink "#/completed" "Completed" <| route == CompletedTodos
        ]


viewLink : String -> String -> Bool -> Html msg
viewLink path label isActive =
    li
        []
        [ a
            [ href path
            , attributeIf isActive <| class "selected"
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