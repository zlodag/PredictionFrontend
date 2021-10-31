module Login exposing (main)

-- MAIN

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, button, dd, div, dl, dt, form, input, label, p, text)
import Html.Attributes exposing (disabled, for, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import Url
import Url.Builder
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key <| LoggedOut Nothing blankCredentials, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Url.Parser.parse routeParser url of
                Just (User id) ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AuthChanged auth ->
            ( { model | auth = auth }, Cmd.none )

        Login credentials ->
            let
                userDecoder : D.Decoder UserData
                userDecoder =
                    D.map3
                        UserData
                        (D.at [ "user", "id" ] D.string |> D.map Id)
                        (D.at [ "user", "name" ] D.string)
                        (D.field "token" D.string)

                expect : Http.Expect Msg
                expect =
                    Http.expectStringResponse resultToMsg <|
                        \response ->
                            case response of
                                Http.BadUrl_ url ->
                                    Err <| text <| "Bad url: " ++ url

                                Http.Timeout_ ->
                                    Err <| text "Timeout error"

                                Http.NetworkError_ ->
                                    Err <| text "Network error"

                                Http.BadStatus_ metadata body ->
                                    let
                                        errorDecoder =
                                            D.map ApiError
                                                (D.field "error" D.string)
                                    in
                                    Err <|
                                        case D.decodeString errorDecoder body of
                                            Ok e ->
                                                dl []
                                                    [ dt [] [ text "Error" ]
                                                    , dd [] [ text e.error ]
                                                    ]

                                            Err _ ->
                                                text <| "Error " ++ String.fromInt metadata.statusCode ++ ": " ++ metadata.statusText

                                Http.GoodStatus_ _ body ->
                                    case D.decodeString userDecoder body of
                                        Ok value ->
                                            Ok value

                                        Err err ->
                                            Err <| text <| D.errorToString err

                resultToMsg : Result (Html Msg) UserData -> Msg
                resultToMsg result =
                    AuthChanged <|
                        case result of
                            Ok user ->
                                LoggedIn user

                            Err error ->
                                LoggedOut (Just error) credentials
            in
            ( { model | auth = LoggingIn credentials }
            , Http.post
                { url = Url.Builder.absolute [ "auth", "login" ] []
                , body = Http.jsonBody <| E.object [ ( "username", E.string credentials.username ), ( "password", E.string credentials.password ) ]
                , expect = expect
                }
            )

        Logout ->
            ( { model | auth = LoggedOut Nothing blankCredentials }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "test"
    , body = viewModel model
    }


type alias Model =
    { key : Nav.Key
    , auth : Auth
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthChanged Auth
    | Login Credentials
    | Logout


type Id
    = Id String


type alias Credentials =
    { username : String
    , password : String
    }


blankCredentials =
    Credentials "" ""


type alias UserData =
    { id : Id
    , name : String
    , token : String
    }


type alias ApiError =
    { error : String
    }


type Auth
    = LoggedOut (Maybe (Html Msg)) Credentials
    | LoggingIn Credentials
    | LoggedIn UserData


type Route
    = Welcome
    | User Id


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Welcome top
        , map (User << Id) <| string
        ]


viewModel : Model -> List (Html Msg)
viewModel model =
    let
        changeUsername credentials newUsername =
            { credentials | username = newUsername }

        changePassword credentials newPassword =
            { credentials | password = newPassword }

        displaySignInForm updateCredentials disableInputs credentials =
            form [ onSubmit <| Login credentials ]
                [ div []
                    [ label [ for "username" ] [ text "Username: " ]
                    , input [ id "username", placeholder "Username", type_ "text", value credentials.username, onInput (changeUsername credentials >> updateCredentials), disabled disableInputs ] []
                    ]
                , div []
                    [ label [ for "password" ] [ text "Password: " ]
                    , input [ id "password", placeholder "Password", type_ "password", value credentials.password, onInput (changePassword credentials >> updateCredentials), disabled disableInputs ] []
                    ]
                , button [ type_ "submit", disabled disableInputs ] [ text "Sign in" ]
                ]
    in
    case model.auth of
        LoggedOut error credentials ->
            displaySignInForm (LoggedOut error >> AuthChanged) False credentials
                :: (case error of
                        Just e ->
                            [ e ]

                        Nothing ->
                            []
                   )

        LoggingIn credentials ->
            [ displaySignInForm (LoggedOut Nothing >> AuthChanged) True credentials ]

        LoggedIn userData ->
            case userData.id of
                Id id ->
                    [ dl []
                        [ dt [] [ text "Username" ]
                        , dd [] [ text userData.name ]
                        , dt [] [ text "ID" ]
                        , dd [] [ text id ]
                        , dt [] [ text "Token" ]
                        , dd [] [ text userData.token ]
                        ]
                    , button [ onClick Logout ] [ text "Log out" ]
                    ]
