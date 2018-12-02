module Messages exposing (update, Msg(..), valueToString)

import Model exposing (Model, Field, Value(..), FieldType(..))
import Regex exposing (Regex)
import List.Extra exposing (getAt, updateAt)
import Maybe exposing (withDefault)
import List.Extra exposing (notMember)
import List.Extra exposing (remove)

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
            if Regex.contains (validRegex (getByIndex m.fields i)) (valueToString (getByIndex m.fields i).value) || st == StringValue "" then
                { m | fields = updateAt i (regexT st) m.fields }
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
    (getAt i m) |> Maybe.withDefault (Field "" "" "" (StringValue "") (RegexField "" False) False [])

validRegex : Field -> Regex
validRegex f =
    takeRegex f
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never

value : Value -> Field -> Field
value v field =
    { field | value = v }

regexT : Value -> Field -> Field
regexT v field =
    { field |
              value = v
            , which = RegexField (takeRegex field) True
            , errors = remove "Regex doesn't match" field.errors
    }

takeRegex : Field -> String
takeRegex m =
    case m.which of
    RegexField st _->
        st
    _ ->
        ""

regexF :  Value -> Field -> Field
regexF v field =
    { field |
              value = v
            , which = RegexField (takeRegex field) False
            , errors = addRegexError field
    }

addRegexError : Field -> List String
addRegexError f =
    if (notMember "Regex doesn't match" f.errors) then
        "Regex doesn't match" :: f.errors
    else
        f.errors