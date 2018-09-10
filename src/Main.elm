module Main exposing (main)
--local imports
import Expr exposing (..)
import InputNum exposing (..)

--
import Dict exposing (Dict)
import Browser
import Debug exposing (toString)
import Html exposing (Html,div,button,text,br)
import Html.Events exposing (onClick)

type Msg
    = Clear
    | Equals
    | ToRegister
    | FromRegister
    | InputNum Int
    | Operator (Expr -> Expr -> Expr)
    | Period

type alias Model =
    { input : InputNum
    , mode : (Expr -> Expr -> Expr)
    , registers : Dict Int Expr
    , acc : Expr
    , period : Bool
    }

init : Model
init =
    { input = ([],[])
    , mode = Add
    , registers = Dict.empty
    , acc = Num 0
    , period = False
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Clear ->
            { model
                | input = ([],[])
                , mode = always
                , acc = Num 0
                , period = False
            }
        Equals ->
            { model
                | input = ([],[])
                , mode = always
                , period = False
                , acc = eval (model.mode model.acc (inputNumToExpr model.input))
            }
        ToRegister ->
            { model
                | input = ([],[])
                , mode = always
                , period = False
                , registers = Dict.update (floor (inputNumToNum model.input)) (always (Just model.acc)) model.registers
                , acc = Num 0
            }
        FromRegister ->
            { model
                | input = ([],[])
                , mode = always
                , period = False
                , acc
                    = Dict.get (floor (inputNumToNum model.input)) model.registers
                    |> Maybe.withDefault (Num 0)
            }
        InputNum x ->
            { model
                | input =
                    case (model.period,model.input) of
                        (False,(xs,ys)) ->
                            ( x :: xs
                            , ys
                            )
                        (True,(xs,ys)) ->
                            ( xs
                            , x :: ys
                            )
            }
        Operator f ->
            { model
                | input = ([],[])
                , mode = f
                , acc = eval (model.mode model.acc (inputNumToExpr model.input))
                , period = False
            }
        Period ->
            { model | period = True }

view : Model ->  Html Msg
view model =
    div []
        [ text (InputNum.toString model.input)
        , br [] []
        , text (Expr.toString model.acc)
        , br [] []
        , button [onClick (InputNum 1)] [text " 1 "]
        , button [onClick (InputNum 2)] [text " 2 "]
        , button [onClick (InputNum 3)] [text " 3 "]
        , button [onClick (Operator Add)] [text " + "]
        , br [] []
        , button [onClick (InputNum 4)] [text " 4 "]
        , button [onClick (InputNum 5)] [text " 5 "]
        , button [onClick (InputNum 6)] [text " 6 "]
        , button [onClick (Operator Sub)] [text " - "]
        , br [] []
        , button [onClick (InputNum 7)] [text " 7 "]
        , button [onClick (InputNum 8)] [text " 8 "]
        , button [onClick (InputNum 9)] [text " 9 "]
        , button [onClick (Operator Mult)] [text " * "]
        , br [] []
        , button [onClick Period] [text " . "]
        , button [onClick (InputNum 0)] [text " 0 "]
        , button [onClick Equals] [text " = "]
        , button [onClick (Operator Div)] [text " / "]
        , br [] []
        , button [onClick Clear] [text " Clear "]
        , br [] []
        , button [onClick ToRegister] [text " to register "]
        , button [onClick FromRegister] [text " from register "]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }