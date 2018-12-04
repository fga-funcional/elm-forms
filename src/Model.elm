module Model exposing (Model, Field, Value(..), FieldType(..), string, number, bool, regexForm, init)

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


init : () -> ( Model, Cmd msg )
init _ =
    ( { fields = [ string "name" "Name" False
            , string "email" "E-mail" True
            , number "age" "Age" True
            , bool "bollean" "Check" False
            , regexForm "real-email" "E-mailR" "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$" True
            ]
      , errors = []
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