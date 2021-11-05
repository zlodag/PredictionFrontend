module Login exposing (Credentials, blankCredentials, displaySignInForm, encodeCredentials)

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Encode as E exposing (Value)


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


displaySignInForm : (Credentials -> msg) -> (Credentials -> msg) -> Bool -> Credentials -> Html msg
displaySignInForm login updateCredentials disableInputs (Credentials username password) =
    let
        changeUsername : Username -> msg
        changeUsername newUsername =
            updateCredentials <| Credentials newUsername password

        changePassword : Password -> msg
        changePassword newPassword =
            updateCredentials <| Credentials username newPassword
    in
    form [ onSubmit <| login <| Credentials username password ]
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
