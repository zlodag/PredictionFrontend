module Login exposing (Credentials, blankCredentials, displaySignInForm, login)

import Common exposing (NamedNodeData, UserInfo)
import Error
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Response(..))
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Predictions.Scalar exposing (Id(..))
import Url.Builder


type Credentials
    = Credentials Username Password


type alias Username =
    String


type alias Password =
    String


blankCredentials : Credentials
blankCredentials =
    Credentials "" ""


encodeCredentials : Credentials -> Value
encodeCredentials (Credentials username password) =
    E.object [ ( "username", E.string username ), ( "password", E.string password ) ]


decodeUser : D.Decoder UserInfo
decodeUser =
    D.map3
        UserInfo
        (D.field "user" <| D.map2 NamedNodeData (D.field "id" <| D.map Id D.string) (D.field "name" D.string))
        (D.field "accessToken" D.string)
        (D.field "refreshToken" D.string)


displaySignInForm : (Credentials -> msg) -> (Credentials -> msg) -> Bool -> Credentials -> Html msg
displaySignInForm loginMsg updateCredentials disableInputs (Credentials username password) =
    let
        changeUsername : Username -> msg
        changeUsername newUsername =
            updateCredentials <| Credentials newUsername password

        changePassword : Password -> msg
        changePassword newPassword =
            updateCredentials <| Credentials username newPassword
    in
    form [ onSubmit <| loginMsg <| Credentials username password ]
        [ div []
            [ label [ for "username" ] [ text "Username: " ]
            , input [ id "username", placeholder "Username", type_ "text", value username, onInput changeUsername, disabled disableInputs ] []
            ]
        , div []
            [ label [ for "password" ] [ text "Password: " ]
            , input [ id "password", placeholder "Password", type_ "password", value password, onInput changePassword, disabled disableInputs ] []
            ]
        , button [ type_ "submit", disabled disableInputs ] [ text "Sign in" ]
        ]


login : (Result (Html msg) UserInfo -> msg) -> Credentials -> Cmd msg
login resultToMsg credentials =
    let
        responseToResult : Response String -> Result (Html msg) UserInfo
        responseToResult response =
            case response of
                GoodStatus_ _ body ->
                    case D.decodeString decodeUser body of
                        Ok value ->
                            Ok value

                        _ ->
                            Err <| Error.responseToHtml response

                _ ->
                    Err <| Error.responseToHtml response
    in
    Http.post
        { url = Url.Builder.absolute [ "auth", "login" ] []
        , body = Http.jsonBody <| encodeCredentials credentials
        , expect = Http.expectStringResponse resultToMsg responseToResult
        }
