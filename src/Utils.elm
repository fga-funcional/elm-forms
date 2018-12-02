
module Utils exposing (ulMap)

{-| Utility functions
-}

import Html exposing (..)

type alias Tag msg =
    List (Attribute msg) -> List (Html msg) -> Html msg

ulMap : (a -> Html msg) -> List a -> Html msg
ulMap =
    htmlMap ul li

htmlMap : Tag msg -> Tag msg -> (a -> Html msg) -> List a -> Html msg
htmlMap parent wrapper func lst =
    let
        wrap f e =
            wrapper [] [ f e ]
    in
    parent [] (List.map (wrap func) lst)
