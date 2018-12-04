module Messages exposing (update, Msg(..), valueToString)

import Model exposing (Model, Field, Value(..), FieldType(..))
import Regex exposing (Regex)
import List.Extra exposing (getAt, updateAt)
import Maybe exposing (withDefault)
import List.Extra exposing (notMember, remove, find)
import List exposing (isEmpty)

type Msg
    = NoOp
    | ValidateForm
    | Input Int Value
    | Regex Int Value


update : Msg -> Model -> Model
update msg m =
    case msg of
        Input i st ->
             { m | fields = updateAt i (verifyIsRequired st) m.fields }
        Regex i st ->
            if Regex.contains (validRegex (getByIndex m.fields i)) (valueToString (getByIndex m.fields i).value) || st == StringValue "" then
                { m | fields = updateAt i (regexT st) m.fields }
            else
                { m | fields = updateAt i (regexF st) m.fields }
        _ ->
            m

verifyIsRequired : Value -> Field -> Field
verifyIsRequired st f =
    if f.isRequired && (st == StringValue "") then
        { f |
                  value = st
                , errors = addRegexError f "Cant be blanck"
        }
    else if not (isEmpty f.errors) then
        { f |
              value = st
            , errors = remove "Cant be blanck" f.errors
        }
    else
        { f | value = st }

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
    if field.isRequired && (v == StringValue "") then
    { field |
              value = v
            , which = RegexField (takeRegex field) True
            , errors = removeAndAdd1 field.errors
    }
    else if  not (isEmpty field.errors) then
        { field |
                  value = v
                , which = RegexField (takeRegex field) False
                , errors = removeTwo field.errors
        }
    else
        { field |
                  value = v
                , which = RegexField (takeRegex field) True
                , errors = remove "Regex doesn't match" field.errors
        }

removeAndAdd1 : List String -> List String
removeAndAdd1 l =
    addError (remove "Regex doesn't match" l) "Cant be blanck"

removeAndAdd2 : List String -> List String
removeAndAdd2 l =
    addError (remove "Cant be blanck" l) "Regex doesn't match"

removeTwo : List String -> List String
removeTwo l =
    remove "Regex doesn't match" (remove "Cant be blanck" l)


addError : List String -> String -> List String
addError l s =
    if (notMember s l) then
        s :: l
    else
        l
    

takeRegex : Field -> String
takeRegex m =
    case m.which of
    RegexField st _->
        st
    _ ->
        ""

regexF :  Value -> Field -> Field
regexF v field =
    if field.isRequired && (v == StringValue "") then      
        { field |
                  value = v
                , which = RegexField (takeRegex field) False
                , errors = addTwoRegexError field "Regex doesn't match" "Cant be blanck"
        }
    else if  not (isEmpty field.errors) then
        { field |
                  value = v
                , which = RegexField (takeRegex field) False
                , errors = removeAndAdd2 field.errors
        }
    else
        { field |
                  value = v
                , which = RegexField (takeRegex field) False
                , errors = addRegexError field "Regex doesn't match"
        }

addRegexError : Field -> String -> List String
addRegexError f s =
    if (notMember s f.errors) then
        s :: f.errors
    else
        f.errors

addTwoRegexError : Field -> String -> String -> List String
addTwoRegexError f s t =
    if (notMember s f.errors) then
        t :: (s :: f.errors)
    else
        f.errors