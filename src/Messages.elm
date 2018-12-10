module Messages exposing (Msg(..), init, update, valueToString)

-- import Json.Decode exposing (list)

import Browser
import Browser.Navigation as Nav
import FormDecoder exposing (fildListDecode)
import FormEncode exposing (encodeForm)
import Http exposing (..)
import List exposing (isEmpty)
import List.Extra exposing (find, getAt, notMember, remove, updateAt)
import Maybe exposing (withDefault)
import Model exposing (..)
import Regex exposing (Regex)
import Url


type Msg
    = NoOp
    | ValidateForm
    | Input Int Value
    | Regex Int Value
    | UrlChanged Url.Url
    | UrlRequest Browser.UrlRequest
    | Name String
    | Label String
    | Bool Bool
    | Type String
    | RegexFormCreate String
    | Response (Result Http.Error String)
    | CreateForm
    | ShowForm
    | FormPostResponse (Result Http.Error ())
    | Password String
    | SendFormAnswer
    | GotFields (Result Http.Error (List Field))


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    ( { fields = []
      , errors = []
      , name = ""
      , label = ""
      , bool = False
      , regex = ""
      , password = ""
      , url = url
      , key = key
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Input i st ->
            ( { m | fields = updateAt i (verifyIsRequired st) m.fields }, Cmd.none )

        Regex i st ->
            if Regex.contains (validRegex (getByIndex m.fields i)) (valueToString (getByIndex m.fields i).value) || st == StringValue "" then
                ( { m | fields = updateAt i (regexT st) m.fields }, Cmd.none )

            else
                ( { m | fields = updateAt i (regexF st) m.fields }, Cmd.none )

        Name st ->
            ( { m | name = st }, Cmd.none )

        Label st ->
            ( { m | label = st }, Cmd.none )

        Bool b ->
            ( { m | bool = b }, Cmd.none )

        RegexFormCreate st ->
            ( { m | regex = st }, Cmd.none )

        CreateForm ->
            ( m, postForm m )

        Password st ->
            ( { m | password = st }, Cmd.none )

        SendFormAnswer ->
            ( m, postAnswer m )
        
        ShowForm ->
            -- Debug.log(Debug.toString(m))
            (m, getForm m)

        GotFields response ->
            case response of
                Ok lfields ->
                    ( { m
                        | fields = lfields
                        , url = m.url
                        , key = m.key
                      }
                    , Cmd.none
                    )

                Err a ->
                    ( { fields = []
                      , errors = []
                      , name = ""
                      , label = ""
                      , bool = False
                      , regex = ""
                      , password = ""
                      , url = m.url
                      , key = m.key
                      }
                    , Cmd.none
                    )

        Type st ->
            case st of
                "Text" ->
                    ( { m | fields = addField m.fields (string m.name m.label m.bool) }, Cmd.none )

                "Numbers" ->
                    ( { m | fields = addField m.fields (number m.name m.label m.bool) }, Cmd.none )

                "Bool" ->
                    ( { m | fields = addField m.fields (bool m.name m.label m.bool) }, Cmd.none )

                "Regex" ->
                    ( { m | fields = addField m.fields (regexForm m.name m.label m.regex m.bool) }, Cmd.none )

                _ ->
                    ( m, Cmd.none )

        _ ->
            ( m, Cmd.none )


addField : List Field -> Field -> List Field
addField l f =
    f :: l


verifyIsRequired : Value -> Field -> Field
verifyIsRequired st f =
    if f.isRequired && (st == StringValue "") then
        { f
            | value = st
            , errors = addRegexError f "Cant be blanck"
        }

    else if not (isEmpty f.errors) then
        { f
            | value = st
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
    getAt i m |> Maybe.withDefault (Field "" "" "" (StringValue "") (RegexField "" False) False [])


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
        { field
            | value = v
            , which = RegexField (takeRegex field) True
            , errors = removeAndAddT field.errors
        }

    else if not (isEmpty field.errors) then
        { field
            | value = v
            , which = RegexField (takeRegex field) False
            , errors = removeTwo field.errors
        }

    else
        { field
            | value = v
            , which = RegexField (takeRegex field) True
            , errors = remove "Regex doesn't match" field.errors
        }


removeAndAddT : List String -> List String
removeAndAddT l =
    addError (remove "Regex doesn't match" l) "Cant be blanck"


removeAndAddF : List String -> List String
removeAndAddF l =
    addError (remove "Cant be blanck" l) "Regex doesn't match"


removeTwo : List String -> List String
removeTwo l =
    remove "Regex doesn't match" (remove "Cant be blanck" l)


addError : List String -> String -> List String
addError l s =
    if notMember s l then
        s :: l

    else
        l


takeRegex : Field -> String
takeRegex m =
    case m.which of
        RegexField st _ ->
            st

        _ ->
            ""


regexF : Value -> Field -> Field
regexF v field =
    if field.isRequired && (v == StringValue "") then
        { field
            | value = v
            , which = RegexField (takeRegex field) False
            , errors = addTwoRegexError field "Regex doesn't match" "Cant be blanck"
        }

    else if not (isEmpty field.errors) then
        { field
            | value = v
            , which = RegexField (takeRegex field) False
            , errors = removeAndAddF field.errors
        }

    else
        { field
            | value = v
            , which = RegexField (takeRegex field) False
            , errors = addRegexError field "Regex doesn't match"
        }


addRegexError : Field -> String -> List String
addRegexError f s =
    if notMember s f.errors then
        s :: f.errors

    else
        f.errors


addTwoRegexError : Field -> String -> String -> List String
addTwoRegexError f s t =
    if notMember s f.errors then
        t :: (s :: f.errors)

    else
        f.errors


postAnswer : Model -> Cmd Msg
postAnswer m =
    let
        headers =
            [ header "Access-Control-Allow-Origin" "*"
            , header "Access-Control-Allow-Methods" "POST, OPTIONS"
            , header "Access-Control-Allow-Headers" "X-Requested-With,content-type"
            , header "Access-Control-Allow-Credentials" "true"
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://dontpad.com/sshthalisson/elm"
        , body = jsonBody (encodeForm m)
        , expect = Http.expectWhatever FormPostResponse
        , timeout = Nothing
        , tracker = Nothing
        }


postForm : Model -> Cmd Msg
postForm m =
    Http.post
        { url = "http://localhost:8080/api" ++ m.url.path
        , body = jsonBody (encodeForm m)
        , expect = Http.expectWhatever FormPostResponse
        }



-- getForm : Model -> Cmd Msg
-- getForm m =
--     Http.get ("http://localhost:8080/api" ++ m.url.path) D.fildListDecode


getForm : Model -> Cmd Msg
getForm m =
    Http.get
        { url = "http://localhost:8080/api" ++ m.url.path
        , expect = Http.expectJson GotFields fildListDecode
        }



-- getFieldRequest : Model -> Http.Request Page
-- getFieldRequest m =
--     Http.get ("http://localhost:8080/api" ++ m.url.path) fildListDecode
