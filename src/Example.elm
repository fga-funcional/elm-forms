module Example exposing (example)

import Model exposing (..)

example : Model
example =
    { init
        | fields =
            [ string "name" "Name" False
            , string "email" "E-mail" True
            , number "age" "Age" True
            , bool "bollean" "Check" False
            , regexForm "real-email" "E-mailR" "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$" True
            ]
    }