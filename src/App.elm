module Main exposing (main)

-- import Browser
-- import Debug
-- import Dict exposing (Dict)
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import List.Extra exposing (getAt, splitAt, updateAt, remove, notMember)
-- import Maybe exposing (withDefault)
-- import Regex exposing (Regex)
-- import Utils exposing (..)

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