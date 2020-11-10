module TodoList exposing
    ( Todo
    , TodoList
    , add
    , clearCompleted
    , complete
    , completeAll
    , delete
    , empty
    , fromJson
    , getActive
    , getAll
    , getCompleted
    , id
    , isCompleted
    , isEmpty
    , label
    , toJson
    , unComplete
    , unCompleteAll
    )

import Json.Decode as Decode
import Json.Encode as Encode
import List
import String


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


empty : TodoList
empty =
    TodoList []


isEmpty : TodoList -> Bool
isEmpty (TodoList list) =
    List.isEmpty list


add : String -> TodoList -> TodoList
add label_ (TodoList todos) =
    let
        newId =
            List.length todos + 1

        newLabel =
            String.trim label_
    in
    TodoList (todos ++ [ TodoRecord newId newLabel Active ])


delete : Int -> TodoList -> TodoList
delete id_ (TodoList todos) =
    TodoList (List.filter ((/=) id_ << .id) todos)


mapTodo : Int -> (TodoRecord -> TodoRecord) -> TodoList -> TodoList
mapTodo id_ action (TodoList todos) =
    TodoList <|
        List.map
            (\todo ->
                if todo.id /= id_ then
                    todo

                else
                    action todo
            )
            todos


complete : Int -> TodoList -> TodoList
complete id_ list =
    mapTodo id_ (\todo -> { todo | status = Completed }) list


unComplete : Int -> TodoList -> TodoList
unComplete id_ list =
    mapTodo id_ (\todo -> { todo | status = Active }) list


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


getAll : TodoList -> List Todo
getAll (TodoList list) =
    toTodos list


getActive : TodoList -> List Todo
getActive (TodoList list) =
    toTodos (List.filter ((==) Active << .status) list)


getCompleted : TodoList -> List Todo
getCompleted (TodoList list) =
    toTodos (List.filter ((==) Completed << .status) list)


isCompleted : Todo -> Bool
isCompleted (Todo { status }) =
    status == Completed


label : Todo -> String
label (Todo todo) =
    todo.label


id : Todo -> Int
id (Todo todo) =
    todo.id


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
        buildTodo id_ label_ completed =
            if completed then
                TodoRecord id_ label_ Completed

            else
                TodoRecord id_ label_ Active
    in
    Decode.map3
        buildTodo
        (Decode.field "id" Decode.int)
        (Decode.field "label" Decode.string)
        (Decode.field "completed" Decode.bool)
        |> Decode.list
        |> Decode.map TodoList
