module Model exposing (Field, FieldType(..), Model, Value(..), bool,  number, regexForm, string)

import Browser.Navigation as Nav
import Url


type alias Model =
    { fields : List Field
    , errors : List String
    , name : String
    , label : String
    , bool : Bool
    , regex : String
    , password : String
    , url : Url.Url
    , key : Nav.Key
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
    = StringField
    | NumberField
    | BoolField
    | RegexField String Bool


string : String -> String -> Bool -> Field
string name label isRequired =
    if isRequired then
        Field name label "" (StringValue "") (StringField ) isRequired [ "Cant be blanck" ]

    else
        Field name label "" (StringValue "") (StringField ) isRequired []


number : String -> String -> Bool -> Field
number name label isRequired =
    if isRequired then
        Field name label "" (StringValue "") (NumberField ) isRequired [ "Cant be blanck" ]

    else
        Field name label "" (StringValue "") (NumberField ) isRequired []


bool : String -> String -> Bool -> Field
bool name label isRequired =
    if isRequired then
        Field name label "" (BoolValue False) BoolField isRequired [ "Cant be blanck" ]

    else
        Field name label "" (BoolValue False) BoolField isRequired []


regexForm : String -> String -> String -> Bool -> Field
regexForm name label regexValue isRequired =
    if isRequired then
        Field name label "" (StringValue "") (RegexField regexValue False) isRequired [ "Cant be blanck" ]

    else
        Field name label "" (StringValue "") (RegexField regexValue False) isRequired []


inf =
    1.0e300
