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
    }


init : Model
init =
    { nextTodoId = 0, todolist = newTodoList, tabIndex = 0 }


type Msg
    = Add Int
    | UpdateTodo Int String
    | ToggleTodoComplete Int
    | HandleKeyboardEvent Int KeyboardEvent


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
                    -- findTodoForId = findTodo todoId
                    toggleTodoListCompleteFunc =
                        toggleTodoListComplete todoId
                in
                { model | todolist = toggleTodoListCompleteFunc model.todolist }

            else if Debug.toString event.keyCode == enterString then
                { model | tabIndex = model.tabIndex + 1, nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist todoId model.nextTodoId model.tabIndex }

            else
                model



-- case event of
--     "CtrlC" ->
--         let
--             -- findTodoForId = findTodo todoId
--             toggleTodoListCompleteFunc =
--                 toggleTodoListComplete todoId
--         in
--         { model | todolist = toggleTodoListCompleteFunc model.todolist }
--     "Enter" ->
--         { model | tabIndex = model.tabIndex + 1, nextTodoId = model.nextTodoId + 1, todolist = addChildToTodolist model.todolist todoId model.nextTodoId model.tabIndex }
--     _ ->
--         model


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


addChildToTodolist : TodoList -> Int -> Int -> Int -> TodoList
addChildToTodolist todolist parentTodoId nextTodoId oldTabIndex =
    case todolist.children of
        Nothing ->
            { todolist | children = Just [ newTodo "" parentTodoId (nextTodoId + 1) (oldTabIndex + 1) ] }

        Just listOfTodos ->
            { todolist | children = Just (listOfTodos ++ [ newTodo "" parentTodoId (nextTodoId + 1) (oldTabIndex + 1) ]) }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill, Background.color blue, paddingXY 0 30 ] <|
        column [ centerX, spacingXY 0 10, width fill, paddingXY 50 0 ]
            [ el [ Font.size 20, Font.color white, paddingXY 10 0 ] (text "Chibs Todo App")
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
        [ row [ width fill, spacing 15 ]
            [ Input.text
                [ Element.htmlAttribute <|
                    on "keydown" <|
                        specialFunction todo.id
                , Border.width 0
                , setTabIndex todo.tabIndex
                , Font.color blue
                , if todo.complete then
                    Font.strike

                  else
                    Font.regular
                ]
                { onChange = UpdateTodo todo.id, placeholder = Just (Input.placeholder [] (text "Enter New Todo Description")), label = Input.labelHidden "", text = todo.description }
            , Input.button [ alignRight, Background.color white, padding 10, Border.rounded 25, Font.color blue ] { label = text "Add Child Todo", onPress = Just (Add todo.id) }
            ]
        , row [ width fill, paddingEach { edges | top = 3 } ]
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
            -- if keyCode == 17 && keyCode == 67 then
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



-- onEnter : msg -> Attribute msg
-- onEnter msg =
--     let
--         isEnterKey event =
--             -- if keyCode == 13 then
--             if Debug.toString event.keyCode == enterString then
--                 Json.succeed msg
--             else
--                 Json.fail "silent failure :)"
--     in
--     Element.htmlAttribute <|
--         on "keyup" <|
--             Json.andThen isEnterKey decodeKeyboardEvent
-- onKeyDown : msg -> Attribute msg
-- onKeyDown msg =
--     Element.htmlAttribute <|
--         on "keydown" <|
--             Json.map handleKeyboardEvent decodeKeyboardEvent
-- handleKeyboardEvent : KeyboardEvent -> msg
-- handleKeyboardEvent event =
--     if event.ctrlKey && Debug.toString event.keyCode == "C" then
--         ctrlCString
--     else if Debug.toString event.keyCode == enterString then
--         enterString
--     else
--         ""
