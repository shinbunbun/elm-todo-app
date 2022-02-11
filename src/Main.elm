module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Todo =
  { id : Int
  , title : String
  }


type alias Model =
  { todo : List Todo, count : Int, inputValue : String }


initialModel : Model
initialModel =
  { todo = [], count = 0, inputValue = "" }


type Msg
  = Add
  | Delete { id : Int }
  | InputValue String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model
        | count = model.count + 1
        , todo = List.append model.todo [ Todo (model.count + 1) model.inputValue ]
        , inputValue = ""
      }

    Delete arg ->
      { model
        | todo = List.filter (\oneTodo -> oneTodo.id /= arg.id) model.todo
      }

    InputValue todoTitle ->
      { model | inputValue = todoTitle }


view : Model -> Html Msg
view model =
  div []
    [ div []
        [ viewInput "text" "Todo" model.inputValue InputValue
        , button [ onClick Add ] [ text "追加" ]
        ]
    , viewTodoItem model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewTodoItem : Model -> Html Msg
viewTodoItem model =
  div [] (List.map viewOneTodo model.todo)


viewOneTodo : Todo -> Html Msg
viewOneTodo todo =
  div []
    [ p [] [ text (String.fromInt todo.id ++ ". " ++ todo.title) ]
    , button [ onClick (Delete { id = todo.id }) ] [ text "削除" ]
    ]


main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
