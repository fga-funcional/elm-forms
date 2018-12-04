module App exposing (main)

import Browser
import Model exposing (..)
import Messages exposing (..)
import CreateForm exposing (..)


-- element
-- aplication

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- SUBSCRIPTIONS -----------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none