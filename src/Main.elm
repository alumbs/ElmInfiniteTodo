module Main exposing (Model, Msg(..), Todo, TodoList, init, main, newTodo, newTodoList, renderTodo, renderTodoList, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (on)
import Json.Decode as Json
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
    { newTodoList | children = Just [ newTodo "" 1 2 ] }


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
    | UpdateTodo Int String
    | ToggleTodoComplete Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add parentTodoId ->
            { model | nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist parentTodoId model.nextTodoId }

        UpdateTodo todoId newValue ->
            if todoId == 1 then
                { model | todolist = updateRootTodoValue model.todolist newValue }

            else
                { model | todolist = updateTodoInList model.todolist todoId newValue }

        ToggleTodoComplete todoId ->
            let
                -- findTodoForId = findTodo todoId
                toggleTodoListCompleteFunc =
                    toggleTodoListComplete todoId
            in
            { model | todolist = toggleTodoListCompleteFunc model.todolist }


updateRootTodoValue todolist newValue =
    let
        oldRoot =
            todolist.root

        newRoot =
            { oldRoot | description = newValue }
    in
    { todolist | root = newRoot }


updateTodoInList : TodoList -> Int -> String -> TodoList
updateTodoInList todolist todoId newTodoValue =
    case todolist.children of
        Nothing ->
            todolist

        Just todos ->
            { todolist | children = Just (updateTodoInListOfTodos todoId newTodoValue todos) }


updateTodoInListOfTodos : Int -> String -> List Todo -> List Todo
updateTodoInListOfTodos todoId newTodoValue listOfTodos =
    let
        updateFunction =
            updateMatchingTodoChild todoId newTodoValue
    in
    List.map updateFunction listOfTodos


updateMatchingTodoChild : Int -> String -> Todo -> Todo
updateMatchingTodoChild todoId newValue singleTodo =
    if todoId == singleTodo.id then
        { singleTodo | description = newValue }

    else
        singleTodo


toggleTodoListComplete idToFind todolist =
    case todolist.children of
        Nothing ->
            todolist

        Just todos ->
            { todolist | children = Just (List.map (toggleTodoComplete idToFind) todos) }


toggleTodoComplete idToFind singleTodo =
    if singleTodo.id == idToFind then
        { singleTodo | complete = not singleTodo.complete }

    else
        singleTodo


addChildToTodolist : TodoList -> Int -> Int -> TodoList
addChildToTodolist todolist parentTodoId nextTodoId =
    case todolist.children of
        Nothing ->
            { todolist | children = Just [ newTodo "" parentTodoId (nextTodoId + 1) ] }

        Just listOfTodos ->
            { todolist | children = Just (listOfTodos ++ [ newTodo "" parentTodoId (nextTodoId + 1) ]) }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill, paddingXY 0 10 ] <|
        column [ centerX, spacingXY 0 10, width fill, paddingXY 50 0 ]
            [ el [ Font.size 20, Font.color (rgb255 240 0 245), paddingXY 10 0 ] (text "Chibs Todo App")
            , renderTodo model.todolist.children model.todolist.root
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
    let
        completeTextString =
            case todo.complete of
                True ->
                    "Mark Not Complete"

                False ->
                    "Mark As Complete"
    in
    column [ paddingEach { edges | left = 10 }, width fill ]
        [ row [ width fill, spaceEvenly ]
            [ Input.text [ onEnter (Add todo.id), Border.width 0 ] { onChange = UpdateTodo todo.id, placeholder = Just (Input.placeholder [] (text "Enter New Todo Description")), label = Input.labelHidden "", text = todo.description }
            , el []
                (text <|
                    "Complete: "
                        ++ Debug.toString todo.complete
                        ++ " "
                        ++ Debug.toString todo.id
                        ++ " "
                )
            , Input.button [] { label = text completeTextString, onPress = Just (ToggleTodoComplete todo.id) }
            , Input.button [] { label = text "Add Child Todo", onPress = Just (Add todo.id) }
            ]
        , row [ width fill, paddingEach { edges | top = 3 } ]
            [ renderTodoList todo.id allTodos ]
        ]


todoBelongsToParent : Int -> Todo -> Bool
todoBelongsToParent parentTodoId todo =
    todo.parentId == parentTodoId


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnterKey keyCode =
            if keyCode == 13 then
                Json.succeed msg

            else
                Json.fail "silent failure :)"
    in
    Element.htmlAttribute <|
        on "keyup" <|
            Json.andThen isEnterKey Html.Events.keyCode
