module FormDecoder exposing (fildListDecode)

import Json.Decode as D
import Model exposing (Field, FieldType(..), Value(..))


fildListDecode : D.Decoder (List Field)
fildListDecode =
  D.list fieldDecoder

fieldDecoder :  D.Decoder Field
fieldDecoder =
  D.map7 Field
    nameDecoder
    labelDecoder
    pDecoder
    vDecoder
    wDecoder
    iDecoder
    eDecoder

nameDecoder : D.Decoder String
nameDecoder =
  D.field "name" D.string

labelDecoder : D.Decoder String
labelDecoder =
  D.field "label" D.string

pDecoder : D.Decoder String
pDecoder =
  D.field "placeholder" D.string

vDecoder : D.Decoder Value
vDecoder =
  D.string
        |> D.andThen (\str ->
            case str of
              "BoolValue" ->
                D.map BoolValue (D.field "value" D.bool)
              "StringValue" ->
                D.map StringValue (D.field "value" D.string)
              somethingElse ->
                D.fail <| "Unknown value: " ++ somethingElse

        )

vDecoder1 : D.Decoder Value
vDecoder1 =
  D.map StringValue
        (D.field "value" D.string)

vStringDecoder : D.Decoder String
vStringDecoder =
  D.field "value" D.string

vBoolDecoder : D.Decoder Bool
vBoolDecoder =
  D.field "value" D.bool


iDecoder : D.Decoder Bool
iDecoder =
  D.field "isRequired" D.bool

eDecoder : D.Decoder (List String)
eDecoder =
  D.field "errors" (D.list D.string)


wDecoder : D.Decoder FieldType
wDecoder =
    D.string
        |> D.andThen (\str ->
            case str of
              "StringField" ->
                D.succeed StringField
              "NumberField" ->
                D.succeed NumberField
              "BoolField" ->
                D.succeed BoolField
              "RegexField" ->
                D.map2 RegexField
                (D.field "regex" D.string)
                (D.field "bool" D.bool)
              somethingElse ->
                D.fail <| "Unknown value: " ++ somethingElse
        )

regexFTDecoder =
  D.field "regex" D.string

boolFTDecoder =
  D.field "bool" D.bool