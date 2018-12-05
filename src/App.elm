module App exposing (main)

import Browser
import Model exposing (..)
import Messages exposing (..)
import CreateForm exposing (..)
-- import View exposing (..)


-- element
-- aplication

main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
        }

-- SUBSCRIPTIONS -----------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none