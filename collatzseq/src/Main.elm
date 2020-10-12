module Main exposing (..)

import Browser
import Html exposing (div, text, pre, button, input)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt, join)

-- Collatz function
collatz: Int -> List Int
collatz n = if (n==1) then
                [1]
            else if (modBy 2 n == 1) then
                n::collatz (n*3+1)
            else 
                n::collatz (n//2)

-- Collatz list function
collatzList: Int -> List (List Int)
collatzList n = List.range 1 n |> List.map collatz

-- Model type def
type alias Model = 
    { number: Maybe Int,
      lists: List (List Int)
    }

-- Initial model
init: Model
init = 
    { number=Nothing
    , lists=[]
    }

-- Main
main = Browser.sandbox 
    { init=init
    , update=update
    , view=view
    }

-- Messages
type Msg = Click | TypedValue String

-- Function to format collatz sequence list as text
format: List (List Int) -> String
format l = l |> 
    List.map (\list -> String.join "," (List.map String.fromInt list)) |> 
    String.join "\n"

-- Update function 
update: Msg -> Model -> Model
update msg model = case msg of
    Click ->
        case model.number of
            Just n ->
                {model | lists=collatzList n}
            Nothing ->
                {model | lists=[]}
    TypedValue txt ->
        {model | number=toInt txt}

-- View function
view model = div []
    [ text "Collatz sequences"
    , div [][]
    , input [type_ "number", onInput TypedValue][]
    , button [onClick Click][text "Calculate"]
    , pre [][text (format model.lists)]
    ]