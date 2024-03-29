module Main exposing (Model, Msg(..), Todo, TodoList, init, main, newTodo, newTodoList, renderTodo, renderTodoList, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (tabindex)
import Html.Events exposing (on)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List exposing (append)


main =
    Browser.sandbox { init = init, update = update, view = view }


blue =
    rgb255 101 157 188


white =
    rgb 1 1 1


darkbrown =
    rgb255 188 152 106


lightyellow =
    rgb255 251 238 193


type alias Todo =
    { id : Int
    , description : String
    , complete : Bool
    , parentId : Int
    , tabIndex : Int
    , minimized : Bool
    }


type alias TodoList =
    { root : Todo
    , children : Maybe (List Todo)
    }


type alias Model =
    { nextTodoId : Int
    , todolist : TodoList
    , tabIndex : Int
    }


ctrlCString =
    "CtrlC"


enterString =
    "Enter"


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


newTodoList =
    { root = newTodo "Root" -1 0 0
    , children = Nothing
    }


newTodoListWithOneChild : TodoList
newTodoListWithOneChild =
    { newTodoList | children = Just [ newTodo "" 1 2 1 ] }


newTodo : String -> Int -> Int -> Int -> Todo
newTodo description parentTodoId nextTodoId oldTabIndex =
    { id = nextTodoId + 1
    , description = description
    , complete = False
    , parentId = parentTodoId
    , tabIndex = oldTabIndex + 1
    , minimized = False
    }


init : Model
init =
    { nextTodoId = 0, todolist = newTodoList, tabIndex = 0 }


type Msg
    = Add Int
    | UpdateTodo Int String
    | ToggleTodoComplete Int
    | HandleKeyboardEvent Int KeyboardEvent
    | ToggleTodoMinimized Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add parentTodoId ->
            { model | tabIndex = model.tabIndex + 1, nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist parentTodoId model.nextTodoId model.tabIndex }

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

        HandleKeyboardEvent todoId event ->
            if event.ctrlKey && Debug.toString event.keyCode == "C" then
                let
                    toggleTodoListCompleteFunc =
                        toggleTodoListComplete todoId
                in
                { model | todolist = toggleTodoListCompleteFunc model.todolist }

            else if Debug.toString event.keyCode == enterString then
                { model | tabIndex = model.tabIndex + 1, nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist todoId model.nextTodoId model.tabIndex }

            else if event.ctrlKey && Debug.toString event.keyCode == "M" then
                let
                    toggleTodoListMinimizedFunc =
                        toggleTodoListMinimized todoId
                in
                { model | todolist = toggleTodoListMinimizedFunc model.todolist }

            else
                model

        ToggleTodoMinimized todoId ->
            let
                toggleTodoListMinimizedFunc =
                    toggleTodoListMinimized todoId
            in
            { model | todolist = toggleTodoListMinimizedFunc model.todolist }


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

    else if singleTodo.parentId == idToFind then
        { singleTodo | complete = not singleTodo.complete }

    else
        singleTodo


toggleTodoListMinimized idToFind todolist =
    case todolist.children of
        Nothing ->
            todolist

        Just todos ->
            { todolist | children = Just (List.map (toggleTodoMinimized idToFind) todos) }


toggleTodoMinimized idToFind singleTodo =
    if singleTodo.id == idToFind then
        { singleTodo | minimized = not singleTodo.minimized }

    else
        singleTodo


addChildToTodolist : TodoList -> Int -> Int -> Int -> TodoList
addChildToTodolist todolist parentTodoId nextTodoId oldTabIndex =
    case todolist.children of
        Nothing ->
            { todolist | children = Just [ newTodo "" parentTodoId (nextTodoId + 1) (oldTabIndex + 1) ] }

        Just listOfTodos ->
            { todolist | children = Just (listOfTodos ++ [ newTodo "" parentTodoId (nextTodoId + 1) (oldTabIndex + 1) ]) }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill, Background.color blue ] <|
        column [ centerX, spacingXY 0 10, width fill, paddingXY 50 30 ]
            [ el [ Font.size 20, Font.color white, paddingXY 10 0 ] (text "Chibs Todo App")
            , renderTodo model.todolist.children model.todolist.root
            , column [ spacing 10, alignBottom, paddingXY 0 30 ]
                [ el [ Font.size 30, Font.color white ] (text "Here are the shortcuts for the application")
                , text "Enter: Creates a new child todo"
                , text "Tab: Goes to the next todo on the page"
                , text "Ctrl + C: Completes the current todo"
                , text "Ctrl + M: Minimizes the current todo"
                ]
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
    column [ paddingEach { edges | left = 10, top = 5 }, width fill ]
        [ row [ width fill, spacing 15 ]
            [ Input.button [ Font.color white, Font.size 30, width (fill |> minimum 15 |> maximum 15), setTabIndex -1 ]
                { label =
                    if todo.minimized then
                        el [ Border.innerGlow lightyellow 1 ] (text "+")

                    else
                        text "-"
                , onPress = Just (ToggleTodoMinimized todo.id)
                }
            , Input.text
                [ Element.htmlAttribute <|
                    on "keydown" <|
                        specialFunction todo.id
                , Border.width 0
                , Font.color blue
                , if todo.complete then
                    Font.strike

                  else
                    Font.regular
                , Font.size 15
                , spacing 0
                , padding 5
                ]
                { onChange = UpdateTodo todo.id, placeholder = Just (Input.placeholder [] (text "Enter New Todo Description")), label = Input.labelHidden "", text = todo.description }
            ]
        , if todo.minimized then
            Element.none

          else
            row [ width fill ]
                [ renderTodoList todo.id allTodos ]
        ]


todoBelongsToParent : Int -> Todo -> Bool
todoBelongsToParent parentTodoId todo =
    todo.parentId == parentTodoId


setTabIndex : Int -> Attribute msg
setTabIndex index =
    Element.htmlAttribute <| tabindex index


specialFunction todoId =
    let
        awesomeFunc =
            HandleKeyboardEvent todoId
    in
    Json.map awesomeFunc decodeKeyboardEvent


onKeyUp : msg -> Attribute msg
onKeyUp msg =
    let
        isEnterKey event =
            if event.ctrlKey && Debug.toString event.keyCode == "C" then
                Json.succeed msg

            else if Debug.toString event.keyCode == enterString then
                Json.succeed msg

            else
                Json.fail "silent failure :)"
    in
    Element.htmlAttribute <|
        on "keyup" <|
            Json.andThen isEnterKey decodeKeyboardEvent
