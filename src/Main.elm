import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font

main =
  Browser.sandbox { init = init , update = update, view = view }

type alias Todo = 
  {
    id: Int,
    description: String,
    complete: Bool,
    parentId: Int
  }

type alias TodoList = 
  {
    root: Todo,
    children: Maybe (List Todo)
  }

type alias Model = 
  {
    nextTodoId: Int,
    todolist: TodoList
  }
  -- {
  --     newtodo: Int,
  --     todos: List String
  -- }

newTodoList = 
  {
    root = newTodo "Root" -1 0,
    children = Nothing
  }

newTodo: String -> Int -> Int -> Todo
newTodo description parentTodoId nextTodoId =
  {
    id = nextTodoId,
    description = description,
    complete = False,
    parentId = parentTodoId
  }

init: Model
init = { nextTodoId = 0, todolist = newTodoList }
  -- ( { newtodo = 0, todos = ["Bob"] }
  -- , Cmd.none
  -- )

type Msg = Add | UpdateTodoId

update: Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model | nextTodoId = model.nextTodoId + 1 }

    UpdateTodoId -> 
      { model | nextTodoId = model.nextTodoId + 1 }
      -- ( { model | todos = "Hi" :: model.todos }
      -- , Cmd.none
      -- )
      -- ( { model | todos.push "New One"}
      -- , Cmd.none
      -- )

    -- Decrement ->
    --   model - 1

view: Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
        [column[][ el [] (text "Chibs Todo App")
        , Input.button [onClick Add] ({label = text "Add New Todo", onPress = Nothing})
        , el [] 
          (column[]
            [ renderTodo model.todolist.root
            , renderTodoChildren model.todolist.children
            -- , column[] (List.map renderTodo (Maybe model.todolist.children ) )
            ]
          )
        ]
        ]
  -- layout []
  --   row [  [] [text ( "Chibs Todo App")]
  --   -- , input [value model.newTodoText] []
  --   , button [ onClick Add ] [text "Add New Todo"]
  --   , div [] (List.map renderTodo model.todos)
  --   -- , div [] [ text (String.fromInt model) ]
  --   -- , button [ onClick Increment ] [ text "+" ]
  --   ]

renderTodoChildren: Maybe (List Todo) -> Element Msg
renderTodoChildren children = 
  case children of
      Nothing ->
        column[] []
  
      Just todoChildren ->
        column[]
        (
          List.map renderTodo todoChildren 
        )
          -- [
          --   el[](text todoVal.description),
          --   el[] 
          --     (text 
          --       <| "Complete" ++ (Debug.toString todoVal.complete) 
          --     )
          -- ]

renderTodo todo =
  column[]
    [
      el[](text todo.description),
      el[] 
        (text 
          <| "Complete: " ++ (Debug.toString todo.complete) 
        )
    ]

-- renderTodo todo =
--   case todo of
--     Just todoVal ->
--       column[]
--         [
--           el[](text todoVal.description),
--           el[] 
--             (text 
--               <| "Complete" ++ (Debug.toString todoVal.complete) 
--             )
--         ]
--     Nothing ->
--       column[][]
    
