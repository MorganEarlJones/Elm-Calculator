module Expr exposing (..)

type Expr
    = Num Float
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr


eval : Expr -> Expr
eval expr =
    let
        eval_ : (Float -> Float -> Float) -> (Expr,Expr) -> Expr
        eval_ operator expressions =
            case expressions of
                (Num x, Num y) ->
                    Num (operator x y)
                (Num x, e2) ->
                    eval_ operator (Num x, eval e2)
                (e1, Num y) ->
                    eval_ operator (eval e1, Num y)
                (e1,e2) ->
                    eval_ operator (eval e1, eval e2)
    in
        case expr of
            Num x ->
                Num x
            Add e1 e2 ->
                eval_ (+) (e1, e2)
            Sub e1 e2 ->
                eval_ (-) (e1, e2)
            Mult e1 e2 ->
                eval_ (*) (e1, e2)
            Div e1 e2 ->
                eval_ (/) (e1, e2)

toString : Expr -> String
toString expr =
    case expr of
        Num x ->
            String.fromFloat x
        Add e1 e2 ->
            toString e1 ++ " + " ++ toString e2
        Sub e1 e2 ->
            toString e1 ++ " - " ++ toString e2
        Mult e1 e2 ->
            toString e1 ++ " * " ++ toString e2
        Div e1 e2 ->
            toString e1 ++ " / " ++ toString e2
