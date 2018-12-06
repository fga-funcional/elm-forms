module Model exposing (Model, Field, Value(..), FieldType(..), string, number, bool, regexForm, init)

import Browser.Navigation as Nav
import Url

type alias Model =
    { fields : List Field
    , errors : List String
    , name : String
    , label : String
    , bool : Bool
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    ( { fields = []
      , errors = []
      , name = ""
      , label = ""
      , bool = False
      }
      , Cmd.none
    )


string : String -> String -> Bool -> Field
string name label isRequired =
    if isRequired then
        Field name label "" (StringValue "") (StringField { maxLength = 2 ^ 30 }) isRequired ["Cant be blanck"]
    else
        Field name label "" (StringValue "") (StringField { maxLength = 2 ^ 30 }) isRequired []


number : String -> String -> Bool -> Field
number name label isRequired =
    if isRequired then
        Field name label "" (StringValue "") (NumberField { range = ( -inf, inf, 0.01 ) }) isRequired ["Cant be blanck"]
    else
        Field name label "" (StringValue "") (NumberField { range = ( -inf, inf, 0.01 ) }) isRequired []


bool : String -> String -> Bool -> Field
bool name label isRequired =
    if isRequired then
        Field name label "" (BoolValue False) BoolField isRequired ["Cant be blanck"]
    else
        Field name label "" (BoolValue False) BoolField isRequired []


regexForm : String -> String -> String  -> Bool -> Field
regexForm name label regexValue isRequired =
    if isRequired then
        Field name label "" (StringValue "") (RegexField regexValue False) isRequired ["Cant be blanck"]
    else
        Field name label "" (StringValue "") (RegexField regexValue False) isRequired []

inf =
    1.0e300