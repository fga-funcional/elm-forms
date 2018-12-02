module Main exposing (main)

import Browser
import Example exposing ( example )
import Model exposing (..)
import Messages exposing (..)
import View exposing (..)

main =
    Browser.sandbox
        { init = example
        , view = view
        , update = update
        }