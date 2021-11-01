module Login exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Error
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet
import Html exposing (Html, a, button, dd, div, dl, dt, form, hr, input, label, li, text, ul)
import Html.Attributes exposing (disabled, for, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..), Resolver)
import Json.Decode as D
import Json.Encode as E
import Predictions.Object.Group
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import Task
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
    parseUrlAndRequest url
        (Model key <| LoggedOut Nothing blankCredentials)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type alias Model =
    { key : Nav.Key
    , auth : Auth
    }


type Auth
    = LoggedOut (Maybe (Html Msg)) Credentials
    | LoggingIn Credentials
    | LoggedIn UserInfo Data


type alias Credentials =
    { username : String
    , password : String
    }


blankCredentials : Credentials
blankCredentials =
    Credentials "" ""


type alias UserInfo =
    { id : Id
    , name : String
    , accessToken : String
    , refreshToken : String
    }


type Data
    = GroupList (HtmlRemoteData GroupListData)
    | GroupDetail (HtmlRemoteData GroupDetailData)
    | UserDetail (HtmlRemoteData NamedNodeData)


type alias HtmlRemoteData a =
    RemoteData (Html Msg) a


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias GroupListData =
    List NamedNodeData


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthChanged Auth
    | Login Credentials
    | Logout


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
            parseUrlAndRequest url model

        AuthChanged auth ->
            ( { model | auth = auth }, Cmd.none )

        Login credentials ->
            let
                userDecoder : D.Decoder UserInfo
                userDecoder =
                    D.map4
                        UserInfo
                        (D.at [ "user", "id" ] D.string |> D.map Id)
                        (D.at [ "user", "name" ] D.string)
                        (D.field "accessToken" D.string)
                        (D.field "refreshToken" D.string)

                responseToResult : Http.Response String -> Result (Html msg) UserInfo
                responseToResult response =
                    case response of
                        Http.GoodStatus_ _ body ->
                            case D.decodeString userDecoder body of
                                Ok value ->
                                    Ok value

                                _ ->
                                    Err <| Error.responseToHtml response

                        _ ->
                            Err <| Error.responseToHtml response

                resultToMsg : Result (Html Msg) UserInfo -> Msg
                resultToMsg result =
                    AuthChanged <|
                        case result of
                            Ok user ->
                                LoggedIn user <| GroupList NotAsked

                            Err error ->
                                LoggedOut (Just error) credentials
            in
            ( { model | auth = LoggingIn credentials }
            , Http.post
                { url = Url.Builder.absolute [ "auth", "login" ] []
                , body = Http.jsonBody <| E.object [ ( "username", E.string credentials.username ), ( "password", E.string credentials.password ) ]
                , expect = Http.expectStringResponse resultToMsg responseToResult
                }
            )

        Logout ->
            ( { model | auth = LoggedOut Nothing blankCredentials }
            , Cmd.none
            )


type Route
    = Welcome
    | Groups
    | Group Id
    | User Id


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Welcome top
        , map Groups <| s "groups"
        , map (Group << Id) <| s "groups" </> string
        , map (User << Id) <| s "users" </> string
        ]


parseUrlAndRequest : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrlAndRequest url model =
    case ( model.auth, Url.Parser.parse routeParser url ) of
        ( _, Just Welcome ) ->
            ( model, Cmd.none )

        ( LoggedIn userData _, Just Groups ) ->
            ( { model | auth = LoggedIn userData (GroupList Loading) }
            , (Predictions.Object.User.groups <| Graphql.SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
                |> Predictions.Query.user { id = userData.id }
                |> sendRequest GroupList userData
            )

        ( LoggedIn userData _, Just (Group groupId) ) ->
            ( { model | auth = LoggedIn userData (GroupDetail Loading) }
            , Graphql.SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                |> Predictions.Object.Group.members
                |> Graphql.SelectionSet.map2 GroupDetailData (Graphql.SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
                |> Predictions.Query.group { id = groupId }
                |> sendRequest GroupDetail userData
            )

        ( LoggedIn userData _, Just (User userId) ) ->
            ( { model | auth = LoggedIn userData (GroupDetail Loading) }
            , Graphql.SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                |> Predictions.Query.user { id = userId }
                |> sendRequest UserDetail userData
            )

        _ ->
            ( { model | auth = LoggedOut Nothing blankCredentials }, Nav.pushUrl model.key <| Url.Builder.absolute [] [] )


api : String
api =
    Url.Builder.absolute [ "api" ] []


sendRequest : (HtmlRemoteData a -> Data) -> UserInfo -> Graphql.SelectionSet.SelectionSet a RootQuery -> Cmd Msg
sendRequest toData originalUser request =
    let
        resultToMessage : Result ( UserInfo, Html Msg ) ( UserInfo, a ) -> Msg
        resultToMessage result =
            let
                ( newUser, remoteData ) =
                    case result of
                        Ok ( user, data ) ->
                            ( user, Success data )

                        Err ( user, errorMsg ) ->
                            ( user, Failure errorMsg )
            in
            remoteData |> toData |> LoggedIn newUser |> AuthChanged
    in
    requestToTask request originalUser
        |> Task.onError (refreshTokenAndTryAgain request)
        |> Task.attempt resultToMessage


requestToTask : Graphql.SelectionSet.SelectionSet a RootQuery -> UserInfo -> Task.Task ( UserInfo, Graphql.Http.Error a ) ( UserInfo, a )
requestToTask request user =
    request
        |> Graphql.Http.queryRequest api
        |> Graphql.Http.withHeader "Authorization" ("Bearer " ++ user.accessToken)
        |> Graphql.Http.toTask
        |> Task.map (Tuple.pair user)
        |> Task.mapError (Tuple.pair user)


refreshTokenAndTryAgain : Graphql.SelectionSet.SelectionSet a RootQuery -> ( UserInfo, Graphql.Http.Error a ) -> Task.Task ( UserInfo, Html Msg ) ( UserInfo, a )
refreshTokenAndTryAgain request ( originalUser, originalError ) =
    let
        onRefresh : Http.Response String -> Result (Html Msg) UserInfo
        onRefresh response =
            case response of
                Http.GoodStatus_ _ body ->
                    D.decodeString
                        (D.map2
                            (UserInfo originalUser.id originalUser.name)
                            (D.field "accessToken" D.string)
                            (D.field "refreshToken" D.string)
                        )
                        body
                        |> Result.mapError (D.errorToString >> text)

                _ ->
                    Err <| Error.responseToHtml response

        fail : Task.Task ( UserInfo, Html Msg ) ( UserInfo, a )
        fail =
            Task.fail ( originalUser, Error.graphqlHttpErrorToHtml originalError )
    in
    case originalError of
        Graphql.Http.HttpError (Graphql.Http.BadStatus _ apiBody) ->
            case D.decodeString (D.field "error" D.string) apiBody of
                Ok "jwt expired" ->
                    Http.task
                        { method = "POST"
                        , headers = []
                        , url = Url.Builder.absolute [ "auth", "refresh" ] []
                        , body = Http.jsonBody <| E.object [ ( "token", E.string originalUser.refreshToken ) ]
                        , resolver = Http.stringResolver onRefresh
                        , timeout = Nothing
                        }
                        |> Task.mapError (Tuple.pair originalUser)
                        |> Task.andThen (requestToTask request >> Task.mapError (Tuple.mapSecond Error.graphqlHttpErrorToHtml))

                _ ->
                    fail

        _ ->
            fail


view : Model -> Document Msg
view model =
    { title = "test"
    , body = viewModel model
    }


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

        LoggedIn userData data ->
            case userData.id of
                Id id ->
                    [ ul []
                        [ li []
                            [ dl []
                                [ dt [] [ text "Username" ]
                                , dd [] [ text userData.name ]
                                , dt [] [ text "ID" ]
                                , dd [] [ text id ]
                                , dt [] [ text "accessToken" ]
                                , dd [] [ text userData.accessToken ]
                                , dt [] [ text "refreshToken" ]
                                , dd [] [ text userData.refreshToken ]
                                ]
                            ]
                        , li [] [ a [ href <| Url.Builder.absolute [ "groups" ] [] ] [ text "Groups" ] ]
                        , li [] [ button [ type_ "button", onClick Logout ] [ text "Log out" ] ]
                        ]
                    , hr [] []
                    , displayData data
                    ]


displayData : Data -> Html Msg
displayData data =
    case data of
        GroupList groups ->
            displayRemoteData displayGroupList groups

        GroupDetail group ->
            displayRemoteData displayGroupDetail group

        UserDetail user ->
            displayRemoteData displayUserDetail user


displayRemoteData : (a -> Html Msg) -> HtmlRemoteData a -> Html Msg
displayRemoteData displayFunction remoteData =
    case remoteData of
        NotAsked ->
            text "Not asked"

        Loading ->
            text "Loading..."

        Failure htmlMsg ->
            htmlMsg

        Success data ->
            displayFunction data


displayGroupList : GroupListData -> Html Msg
displayGroupList groups =
    let
        toHref : Id -> String
        toHref (Id id) =
            Url.Builder.absolute [ "groups", id ] []
    in
    dl []
        [ dt [] [ text <| "Groups: " ++ (String.fromInt <| List.length groups) ]
        , dd [] [ ul [] <| List.map (displayNamedNode toHref >> List.singleton >> li []) groups ]
        ]


displayGroupDetail : GroupDetailData -> Html msg
displayGroupDetail group =
    dl []
        [ dt [] [ text "Group" ]
        , dd [] [ displayNamedNode groupUrl group.node ]
        , dt [] [ text <| "Members: " ++ (String.fromInt <| List.length group.members) ]
        , dd [] [ ul [] <| List.map (displayNamedNode userUrl >> List.singleton >> li []) group.members ]
        ]


displayUserDetail : NamedNodeData -> Html msg
displayUserDetail user =
    displayNamedNode userUrl user


displayNamedNode : (Id -> String) -> NamedNodeData -> Html msg
displayNamedNode toUrl node =
    a [ node.id |> toUrl |> href ] [ text node.name ]


userUrl : Id -> String
userUrl (Id id) =
    Url.Builder.absolute [ "users", id ] []


groupUrl : Id -> String
groupUrl (Id id) =
    Url.Builder.absolute [ "groups", id ] []
