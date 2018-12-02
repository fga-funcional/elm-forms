module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Messages exposing (Msg(..), valueToString)
import Utils exposing (ulMap)

view : Model -> Html Msg
view m =
    let
        validation =
            if isValid m then
                "(ok)"
            else
                "(invalid)"
    in
    div []
        [ h1 [] [ text "Form", text validation ]
        , ulMap text m.errors
        , div [] (List.indexedMap viewField m.fields)
        , button [] [ text "Validate" ]
        , button [] [ text "Submit" ]
        , h3 [] [ text "Raw data" ]
        , code [] [ text (Debug.toString m) ]
        ]


viewField : Int -> Field -> Html Msg
viewField i field =
    let
        attributes =
            case field.which of
                NumberField _ ->
                    [ type_ "number" ]

                BoolField ->
                    [ type_ "checkbox" ]

                RegexField _ b ->
                    if (valueToString field.value) == "" then
                        []
                    else
                        if not b then
                            [style "background-color" "red"]
                        else
                            []
                _ ->
                    []
    in
    div []
        [ label [] [ text field.label ]
        , input
            ([ placeholder field.placeholder
             , Html.Attributes.value (stringFromValue field.value)
             , inputField i field
             ]
                ++ attributes
            )
            []
        ]


stringFromValue : Value -> String
stringFromValue v =
    case v of
        StringValue st ->
            st

        BoolValue b ->
            "False"

isValid : Model -> Bool
isValid m =
    List.isEmpty m.errors && List.all (.errors >> List.isEmpty) m.fields


valueFromString : FieldType -> String -> Value
valueFromString which st =
    case which of
        BoolField ->
            BoolValue False

        _ ->
            StringValue st


inputField : Int -> Field -> Attribute Msg
inputField i field =
    case field.which of
        BoolField ->
            onClick (Input i (toggleValue field.value))

        RegexField _ _->
            onInput (valueFromString field.which >> Regex i)

        _ ->
            onInput (valueFromString field.which >> Input i)


toggleValue : Value -> Value
toggleValue v =
    case v of
        BoolValue b ->
            BoolValue (not b)

        _ ->
            v