port module TodoApp exposing (..)
import Dict exposing (update)
import Browser
import Debug exposing (log)

--import Browser
import Html as Ht exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as JD 


port getUniqueId : String -> Cmd msg
port receiveId : (String -> msg) -> Sub msg

type Msg = 
    CreateTodo String
    | OnSubmit
    | InputChanged String
    | Toggle Todo
    | DeleteTodo Todo
    | GetUuid String

type alias Todo = 
    {isCompleted: Bool
    ,taskName: String
    ,id: String
    }

type ErrorMsg = 
    ErrorMsg String 
    | NoError


type alias Model = 
    { todos: List Todo
    , errorMsg: ErrorMsg
    , todo: String
    , userInput: String }



view model = 
    div [ class "root"] 
        [div [class "content"]
            [ h1 [class "title"] [text "Todo App"]
            , viewForm model
            , viewListWrapper model
            ]
        ]
 
viewListWrapper : Model -> Html Msg
viewListWrapper model = 
    div [class "lists-wrapper"] 
                [div [class "todo-list"] 
                    [h2 [] [text "Todo:"]
                    ,div [] (viewTodoList model.todos)
                    ]
                ,div [class "completed-list"] 
                    [h2 [] [text "Completed:"]
                    , div [] (viewCompletedList model.todos)
                    ]
                ]


viewForm : Model -> Html Msg 
viewForm model = 
    Ht.form [class "create-todo-form",onSubmit OnSubmit ] 
                [h2 [] [text "Create a New Todo"]
                ,input [class "todo-name"
                       ,type_ "text"
                       , placeholder "Enter todo description"
                       , autocomplete True
                       , onInput InputChanged
                       , value model.userInput] []
                , viewError model
                , button [class "submit-btn"
                         , type_ "submit"]
                         [text "Create Todo"]
                ]

            
viewError : Model -> Html Msg 
viewError model = 
    case model.errorMsg of 
        ErrorMsg err -> p [] [text err]
        NoError -> p [] []

viewItem: Todo -> Html Msg
viewItem todo = 
    div [class "todo-item", onClick (Toggle todo)] 
        [span [] [text todo.taskName]
        ,button [onDeleteTodo (DeleteTodo todo)] [text "X"]]

viewTodoList: List Todo -> List (Html Msg)
viewTodoList items = 
    let todos = List.filter notDone items
        notDone todo = todo.isCompleted == False
    in
    List.map viewItem todos

viewCompletedList: List Todo -> List (Html Msg)
viewCompletedList items = 
    let completed = List.filter isDone items
        isDone item = item.isCompleted == True
    in
    List.map viewItem completed

initialModel : Model 
initialModel = 
    { todos =
        [{isCompleted= False
         ,taskName= "Play Guitar"
         ,id= "23232"
         }
        ,{isCompleted=False
         ,taskName = "Program"
         ,id="3233"
         }
        ,{isCompleted=True
        ,taskName = "Do Homework"
        ,id= "3432"}
        ]
    , errorMsg= NoError
    , todo = ""
    , userInput = ""
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        CreateTodo todo -> 
            ({ model | todo=todo} ,Cmd.none)
        OnSubmit -> 
                if (isTaskable model) then ({model | errorMsg=NoError}, getUniqueId "Blank")
                else ({model | errorMsg= (errorMsg model), userInput= ""}, Cmd.none)
        GetUuid uuid-> 
            ({ model | userInput=""
                     , todos= (makeTodo (String.trim model.userInput) uuid)::model.todos}, Cmd.none)
        InputChanged inp -> 
            ({model | userInput=inp}, Cmd.none)
        Toggle todo->
            ({model | todos = updateTodoList todo model.todos }, Cmd.none)
        DeleteTodo todo -> 
            ({model | todos = deleteTodo todo model.todos},Cmd.none)





errorMsg : Model -> ErrorMsg
errorMsg model= 
    if (isTaskable model) then NoError
    else if (String.trim model.userInput) == "" then ErrorMsg "The input for the user is blank"
    else ErrorMsg "This is already a task!"



isTaskable : Model -> Bool 
isTaskable model = List.all (\todo -> todo.taskName /= model.userInput && (String.trim model.userInput) /="") model.todos

toggleIsCompleted : Todo -> Todo 
toggleIsCompleted todo = 
    { todo | isCompleted= not todo.isCompleted}

updateTodoList : Todo -> List Todo -> List Todo 
updateTodoList completedtodo todos = 
    let filteredTodos = List.filter (\todo -> todo.taskName /= updatedTodo.taskName) todos
        updatedTodo = toggleIsCompleted completedtodo
    in 
    updatedTodo :: filteredTodos


deleteTodo : Todo -> List Todo -> List Todo
deleteTodo deletedTodo todos = List.filter (\todo -> todo.taskName /= deletedTodo.taskName) todos

makeTodo : String -> String -> Todo 
makeTodo todoName uuid = 
    { isCompleted= False
    , taskName = todoName
    , id = uuid
    }




init : () -> (Model, Cmd Msg)
init _ = 
    ( initialModel, Cmd.none)

main : Program () Model Msg
main = --This returns ( Model, Cmd Msg)
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- stopPropagationOn : String -> Json.Decoder (msg, Bool) -> Attribute msg
-- stopPropagationOn event decoder =
--   VirtualDom.on event (VirtualDom.MayStopPropagation decoder)

-- on : String -> Json.Decoder msg -> Attribute msg
-- on event decoder =
--   VirtualDom.on event (VirtualDom.Normal decoder)

-- onSubmit : msg -> Attribute msg
-- onSubmit msg =
--   preventDefaultOn "submit" (Json.map alwaysPreventDefault (Json.succeed msg))


-- alwaysPreventDefault : msg -> ( msg, Bool )
-- alwaysPreventDefault msg =
--   ( msg, True )

-- onInput : (String -> msg) -> Attribute msg
-- onInput tagger =
--   stopPropagationOn "input" (Json.map alwaysStop (Json.map tagger targetValue))


onDeleteTodo : msg -> Attribute msg
onDeleteTodo msg = 
    stopPropagationOn "click" (JD.map alwaysStop (JD.succeed msg))


alwaysStop : a -> (a, Bool)
alwaysStop x =
  (x, True)

-- alwaysStop : a -> (a, Bool)
-- alwaysStop x =
--   (x, True)

-- onClick : msg -> Attribute msg
-- onClick msg =
--   on "click" (Json.succeed msg)


-- on : String -> Json.Decoder msg -> Attribute msg
-- on event decoder =
--   VirtualDom.on event (VirtualDom.Normal decoder)


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveId (GetUuid)