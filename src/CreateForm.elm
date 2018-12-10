module CreateForm exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L exposing (isEmpty)
import Messages exposing (Msg(..), valueToString)
import Model exposing (..)
import Utils exposing (ulMap)


view : Model -> Browser.Document Msg
view m =
    let
        validation =
            if isValid m then
                "(ok)"

            else
                "(invalid)"

        -- isEmpty =
        --     if L.isEmpty m.fields then
        --         False
        --     else
        --         True
    in
    { title = "ElmForm"
    , body =
        [ h1 [] [ text "Form", text validation ]
        , renderCreateForm m
        , ulMap text m.errors
        , div [] (List.indexedMap viewField m.fields)
        , label [] [ text "Digite uma senha para recuperar os dados do form" ]
        , input [ onInput Password, type_ "password" ] []
        , button [ onClick CreateForm ] [ text "Criar" ]
        , button [ onClick ShowForm ] [ text "Verificar se ja exist form" ]
        ]
    }


renderCreateForm m =
    div []
        [ label [] [ text "Name" ]
        , input [ onInput Name ] []
        , label [] [ text "label" ]
        , input [ onInput Label ] []
        , label [] [ text "obrigatório" ]
        , input [ onClick (Bool (isRequired m.bool)), type_ "checkbox" ] []
        , button [ onClick (Type "Text") ] [ text "Texto" ]
        , button [ onClick (Type "Numbers") ] [ text "Numeros" ]
        , button [ onClick (Type "Bool") ] [ text "Boleano" ]
        , label [] [ text "Regex" ]
        , input [ onInput RegexFormCreate ] []
        , button [ onClick (Type "Regex") ] [ text "Regex" ]
        ]


isRequired : Bool -> Bool
isRequired b =
    not b


viewField : Int -> Field -> Html Msg
viewField i field =
    let
        attributes =
            case field.which of
                NumberField ->
                    [ type_ "number" ]

                BoolField ->
                    [ type_ "checkbox" ]

                RegexField _ b ->
                    if valueToString field.value == "" then
                        []

                    else if not b then
                        [ style "background-color" "red" ]

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

        RegexField _ _ ->
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
