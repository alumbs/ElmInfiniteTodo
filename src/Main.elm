module Main exposing (Model, Msg(..), Todo, TodoList, init, main, newTodo, newTodoList, renderTodo, renderTodoChildren, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List exposing (append)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Todo =
    { id : Int
    , description : String
    , complete : Bool
    , parentId : Int
    }


type alias TodoList =
    { root : Todo
    , children : Maybe (List Todo)
    }


type alias Model =
    { nextTodoId : Int
    , todolist : TodoList
    }


newTodoList =
    { root = newTodo "Root" -1 0
    , children = Nothing
    }


newTodoListWithOneChild : TodoList
newTodoListWithOneChild =
    { newTodoList | children = Just [ newTodo "New" 1 2 ] }


newTodo : String -> Int -> Int -> Todo
newTodo description parentTodoId nextTodoId =
    { id = nextTodoId + 1
    , description = description
    , complete = False
    , parentId = parentTodoId
    }


init : Model
init =
    { nextTodoId = 0, todolist = newTodoList }


type Msg
    = Add Int
    | UpdateTodoId
    | Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add parentTodoId ->
            { model | nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist parentTodoId model.nextTodoId }

        UpdateTodoId ->
            { model | nextTodoId = model.nextTodoId + 1 }

        Noop ->
            model


addChildToTodolist : TodoList -> Int -> Int -> TodoList
addChildToTodolist todolist parentTodoId nextTodoId =
    case todolist.children of
        Nothing ->
            { todolist | children = Just [ newTodo "New" parentTodoId (nextTodoId + 1) ] }

        Just listOfTodos ->
            { todolist | children = Just (newTodo "New" parentTodoId (nextTodoId + 1) :: listOfTodos) }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill, paddingXY 0 10 ] <|
        column [ centerX, spacingXY 0 10 ]
            [ el [ Font.size 20, Font.color (rgb255 240 0 245) ] (text "Chibs Todo App")

            -- , Input.button [] { label = text "Add New Todo", onPress = Nothing }
            , renderTodo model.todolist.root
            , renderTodoChildren model.todolist.children
            ]


renderTodoChildren : Maybe (List Todo) -> Element Msg
renderTodoChildren children =
    case children of
        Nothing ->
            none

        Just todoChildren ->
            column []
                (List.map renderTodo todoChildren)


renderTodo : Todo -> Element Msg
renderTodo todo =
    row [ paddingXY 10 0 ]
        [ el [] (text (todo.description ++ Debug.toString todo.id))
        , el []
            (text <|
                "Complete: "
                    ++ Debug.toString todo.complete
            )

        -- , Input.button [] { onPress = Nothing, label = "Add Child Todo" }
        , Input.button [] { label = text "Add Child Todo", onPress = Just (Add todo.id) }
        ]
