module Form exposing (Model, Msg, init, main, update, view)

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt, splitAt, updateAt, remove)
import Maybe exposing (withDefault)
import Regex exposing (Regex)
import Utils exposing (..)


main =
    Browser.sandbox
        { init = example
        , view = view
        , update = update
        }



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { fields : List Field
    , errors : List String
    }


type alias Field =
    { name : String
    , label : String
    , placeholder : String
    , value : Value
    , which : FieldType
    , errors : List String
    }


type Value
    = BoolValue Bool
    | StringValue String


type FieldType
    = StringField { maxLength : Int }
    | NumberField { range : ( Float, Float, Float ) }
    | BoolField
    | RegexField String Bool


init : Model
init =
    { fields = [], errors = [] }


string : String -> String -> Field
string name label =
    Field name label "" (StringValue "") (StringField { maxLength = 2 ^ 30 }) []


number : String -> String -> Field
number name label =
    Field name label "" (StringValue "") (NumberField { range = ( -inf, inf, 0.01 ) }) []


bool : String -> String -> Field
bool name label =
    Field name label "" (BoolValue False) BoolField []


regexForm : String -> String -> String -> Field
regexForm name label regex_value =
    Field name label "" (StringValue "") (RegexField regex_value False) []


inf =
    1.0e300


isValid : Model -> Bool
isValid m =
    List.isEmpty m.errors && List.all (.errors >> List.isEmpty) m.fields


value : Value -> Field -> Field
value v field =
    { field | value = v }

regexT : Value -> Field -> Field
regexT v field =
    { field |
              value = v 
            , which = RegexField (takeRegex field) True
            , errors = remove "Invalid Email" field.errors
    }

regexF :  Value -> Field -> Field
regexF v field =
    { field |
              value = v 
            , which = RegexField (takeRegex field) False
            , errors = "Regex doesn't match" :: field.errors
    }



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | ValidateForm
    | Input Int Value
    | Regex Int Value


update : Msg -> Model -> Model
update msg m =
    case msg of
        Input i st ->
            { m | fields = updateAt i (value st) m.fields }
        Regex i st ->
            if Regex.contains (validRegex (getByIndex m.fields i)) (valueToString (getByIndex m.fields i).value) then
                { m |
                    fields = updateAt i (regexT st) m.fields }
            else
                { m | fields = updateAt i (regexF st) m.fields }
        _ ->
            m

valueToString : Value -> String
valueToString v = 
    case v of
    StringValue st ->
        st
    _ ->
        ""

getByIndex : List Field -> Int -> Field
getByIndex m i =
    (getAt i m) |> Maybe.withDefault (Field "" "" "" (StringValue "") (RegexField "" False) [])

validRegex : Field -> Regex
validRegex f =
    takeRegex f
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never

takeRegex : Field -> String
takeRegex m =
    case m.which of
    RegexField st _->
        st
    _ ->
        ""


--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


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



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | fields =
            [ string "name" "Name"
            , string "email" "E-mail"
            , number "age" "Age"
            , bool "bollean" "Check"
            , regexForm "real-email" "E-mailR" "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
            ]
    }
