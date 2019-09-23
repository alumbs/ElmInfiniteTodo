module Main exposing (Model, Msg(..), Todo, TodoList, init, main, newTodo, newTodoList, renderTodo, renderTodoList, update, view)

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


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
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
            { todolist | children = Just (listOfTodos ++ [ newTodo "New" parentTodoId (nextTodoId + 1) ]) }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill, paddingXY 0 10 ] <|
        column [ centerX, spacingXY 0 10, width fill, paddingXY 50 0 ]
            [ el [ Font.size 20, Font.color (rgb255 240 0 245), paddingXY 10 0 ] (text "Chibs Todo App")
            , renderTodo model.todolist.children model.todolist.root

            -- , renderTodoList model.todolist.root.id model.todolist.children
            ]


renderTodoList : Int -> Maybe (List Todo) -> Element Msg
renderTodoList parentTodoId children =
    case children of
        Nothing ->
            none

        Just todoChildren ->
            let
                filterFunction =
                    todoBelongsToParent parentTodoId

                renderTodoFunction =
                    renderTodo children

                filteredTodos =
                    List.filter filterFunction todoChildren
            in
            column [ paddingEach { edges | left = 10 }, width fill ]
                (List.map renderTodoFunction filteredTodos)


renderTodo : Maybe (List Todo) -> Todo -> Element Msg
renderTodo allTodos todo =
    column [ paddingEach { edges | left = 10 }, width fill ]
        [ row [ width fill, spaceEvenly ]
            [ el [] (text (todo.description ++ Debug.toString todo.id))
            , el []
                (text <|
                    "Complete: "
                        ++ Debug.toString todo.complete
                )
            , Input.button [] { label = text "Add Child Todo", onPress = Just (Add todo.id) }
            ]
        , row [ width fill ]
            [ renderTodoList todo.id allTodos ]
        ]


todoBelongsToParent : Int -> Todo -> Bool
todoBelongsToParent parentTodoId todo =
    todo.parentId == parentTodoId
