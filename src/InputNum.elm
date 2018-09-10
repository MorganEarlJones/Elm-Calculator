module InputNum exposing (..)

import Expr exposing (..)

type alias InputNum = (List Int, List Int)

listNumToNum : List Int -> Float
listNumToNum ls =
    let
        listNumToNum_ : Int -> List Int -> Float
        listNumToNum_ digit ls_ =
            case ls_ of
                [] ->
                    0
                (x::xs) ->
                    toFloat (x * digit) + listNumToNum_ (digit * 10) xs
    in
        listNumToNum_ 1 ls

inputNumToNum : InputNum -> Float
inputNumToNum input = 
    case input of
        ([],[]) ->
            0
        (x::xs,[]) ->
            listNumToNum (x::xs)
        ([],(y::ys)) ->
            listNumToNum (y::ys) / toFloat (10 ^ List.length (y::ys))
        (x::xs,y::ys) ->
            listNumToNum (x::xs) + (listNumToNum (y::ys) / toFloat (10 ^ List.length (y::ys)))

inputNumToExpr : InputNum -> Expr
inputNumToExpr input = Num (inputNumToNum input)

toString : InputNum -> String
toString input =
    case input of
        ([],[]) ->
            "_"
        _ ->
            String.fromFloat (inputNumToNum input)
