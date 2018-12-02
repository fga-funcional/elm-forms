module Model exposing (Model, Field, Value(..), FieldType(..), takeRegex, regexT, regexF, value, string, number, bool, regexForm, init)

import List.Extra exposing (notMember)
import List.Extra exposing (remove)

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
    , isRequired : Bool
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


string : String -> String -> Bool -> Field
string name label isRequired =
    Field name label "" (StringValue "") (StringField { maxLength = 2 ^ 30 }) isRequired []


number : String -> String -> Bool -> Field
number name label isRequired =
    Field name label "" (StringValue "") (NumberField { range = ( -inf, inf, 0.01 ) }) isRequired []


bool : String -> String -> Bool -> Field
bool name label isRequired =
    Field name label "" (BoolValue False) BoolField isRequired []


regexForm : String -> String -> String  -> Bool -> Field
regexForm name label regexValue isRequired =
    Field name label "" (StringValue "") (RegexField regexValue False) isRequired []


inf =
    1.0e300

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