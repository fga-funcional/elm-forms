module FormEncode exposing (encodeForm)

import Json.Encode as Encode
import Model exposing (..)


encodeForm : Model -> Encode.Value
encodeForm m =
    Encode.object
        [ ( "fields", Encode.list formEncoder m.fields )
        , ( "password", Encode.string m.password )
        ]



-- musicEncoder : D.Encoder Music


formEncoder : Field -> Encode.Value
formEncoder f =
    -- case lst of
    --     [] ->
    --         []
    --     x :: xs ->
            Encode.object
                [ ( "name", Encode.string f.name )
                , ( "label", Encode.string f.label )
                , ( "placeholder", Encode.string f.placeholder )
                , ( "value", valueEconder f.value )
                , ( "which", whichEncoder f.which  )
                , ( "isRequired", Encode.bool f.isRequired )
                , ( "errors", (Encode.list Encode.string) f.errors )
                ]
            -- formEncoder xs


valueEconder : Value -> Encode.Value
valueEconder v =
    case v of
        BoolValue b ->
            Encode.bool b

        StringValue st ->
            Encode.string st


whichEncoder : FieldType -> Encode.Value
whichEncoder ft =
    case ft of
        StringField ->
            Encode.object [ ( "type", Encode.string "StringField" ) ]

        NumberField ->
            Encode.object [ ( "type", Encode.string "NumberField" ) ]

        BoolField ->
            Encode.object [ ( "type", Encode.string "BoolField" ) ]

        RegexField st b ->
            Encode.object [ ( "type", Encode.string "RegexField" ), ("regex", Encode.string st), ( "bool", Encode.bool b ) ]


inf =
    1.0e300
