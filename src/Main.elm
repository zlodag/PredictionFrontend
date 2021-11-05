module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Common exposing (NamedNodeData, Now, UserInfo, displayNamedNode, groupUrl, userUrl)
import Error
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, dd, div, dl, dt, hr, li, p, text, ul)
import Html.Attributes exposing (href, type_)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Resolver)
import Json.Decode as D
import Json.Encode as E
import Login exposing (Credentials, blankCredentials, viewSignInForm)
import MyDetails
import Predictions.Object.Group
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import Task
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)
import UserScore


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
    parseUrlAndRequest url <| Model key (Now Time.utc <| Time.millisToPosix 0) (LoggedOut Nothing blankCredentials)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type alias Model =
    { key : Nav.Key
    , now : Now
    , auth : Auth
    }


type Auth
    = LoggedOut (Maybe (Html Msg)) Credentials
    | LoggingIn Credentials
    | LoggedIn UserInfo Data


type Data
    = NoData
    | Me (HtmlRemoteData MyDetails.Data)
    | GroupList (HtmlRemoteData GroupListData)
    | GroupDetail (HtmlRemoteData GroupDetailData)
    | UserDetail (HtmlRemoteData NamedNodeData)
    | UserScore (HtmlRemoteData UserScore.Data)


type alias HtmlRemoteData a =
    RemoteData (Html Msg) a


type alias GroupListData =
    List NamedNodeData


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    }


type Msg
    = NewTime Now
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoggedInOK UserInfo
    | DataUpdated Auth
    | Login Credentials
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime now ->
            ( { model | now = now }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            parseUrlAndRequest url model

        LoggedInOK user ->
            ( { model | auth = LoggedIn user NoData }
              --, Nav.pushUrl model.key <| userUrl user.id
            , Cmd.none
            )

        DataUpdated auth ->
            ( { model | auth = auth }, Task.map2 Now Time.here Time.now |> Task.perform NewTime )

        Login credentials ->
            let
                resultToMsg : Result (Html Msg) UserInfo -> Msg
                resultToMsg result =
                    case result of
                        Ok user ->
                            LoggedInOK user

                        Err error ->
                            LoggedOut (Just error) credentials |> DataUpdated
            in
            ( { model | auth = LoggingIn credentials }
            , Login.login resultToMsg credentials
            )

        Logout ->
            ( { model | auth = LoggedOut Nothing blankCredentials }
            , Cmd.none
            )


type Route
    = Welcome
    | UserDetailRoute Id
    | UserScoreRoute Id
    | GroupsRoute
    | GroupDetailRoute Id


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Welcome top
        , map GroupsRoute <| s "groups"
        , map (GroupDetailRoute << Id) <| s "groups" </> string
        , map (UserDetailRoute << Id) <| s "users" </> string
        , map (UserScoreRoute << Id) <| s "user" </> string </> s "score"
        ]


parseUrlAndRequest : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrlAndRequest url model =
    case ( model.auth, Url.Parser.parse routeParser url ) of
        ( LoggedIn userData _, Just Welcome ) ->
            ( { model | auth = LoggedIn userData NoData }, Cmd.none )

        ( _, Just Welcome ) ->
            ( model, Cmd.none )

        ( LoggedIn userData _, Just (UserDetailRoute userId) ) ->
            if userId == userData.node.id then
                ( { model | auth = LoggedIn userData (Me Loading) }
                , MyDetails.selectionSet userId
                    |> sendRequest Me userData
                )

            else
                ( { model | auth = LoggedIn userData (UserDetail Loading) }
                , SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                    |> Predictions.Query.user { id = userId }
                    |> sendRequest UserDetail userData
                )

        ( LoggedIn userData _, Just GroupsRoute ) ->
            ( { model | auth = LoggedIn userData (GroupList Loading) }
            , SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name
                |> Predictions.Object.User.groups
                |> Predictions.Query.user { id = userData.node.id }
                |> sendRequest GroupList userData
            )

        ( LoggedIn userData _, Just (GroupDetailRoute groupId) ) ->
            ( { model | auth = LoggedIn userData (GroupDetail Loading) }
            , SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                |> Predictions.Object.Group.members
                |> SelectionSet.map2 GroupDetailData (SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
                |> Predictions.Query.group { id = groupId }
                |> sendRequest GroupDetail userData
            )

        ( LoggedIn userData _, Just (UserScoreRoute userId) ) ->
            ( { model | auth = LoggedIn userData (UserScore Loading) }
            , UserScore.selectionSet userId
                |> sendRequest UserScore userData
            )

        _ ->
            ( model, Nav.pushUrl model.key "/" )


api : String
api =
    Url.Builder.absolute [ "api" ] []


sendRequest : (HtmlRemoteData a -> Data) -> UserInfo -> SelectionSet a RootQuery -> Cmd Msg
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
            remoteData |> toData |> LoggedIn newUser |> DataUpdated
    in
    requestToTask request originalUser
        |> Task.onError (refreshTokenAndTryAgain request)
        |> Task.attempt resultToMessage


requestToTask : SelectionSet a RootQuery -> UserInfo -> Task.Task ( UserInfo, Graphql.Http.Error a ) ( UserInfo, a )
requestToTask request user =
    request
        |> Graphql.Http.queryRequest api
        |> Graphql.Http.withHeader "Authorization" ("Bearer " ++ user.accessToken)
        |> Graphql.Http.toTask
        |> Task.map (Tuple.pair user)
        |> Task.mapError (Tuple.pair user)


refreshTokenAndTryAgain : SelectionSet a RootQuery -> ( UserInfo, Graphql.Http.Error a ) -> Task.Task ( UserInfo, Html Msg ) ( UserInfo, a )
refreshTokenAndTryAgain request ( originalUser, originalError ) =
    let
        onRefresh : Http.Response String -> Result (Html Msg) UserInfo
        onRefresh response =
            case response of
                Http.GoodStatus_ _ body ->
                    D.decodeString
                        (D.map2
                            (UserInfo originalUser.node)
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
    { title = "Predictions"
    , body = viewModel model
    }


viewModel : Model -> List (Html Msg)
viewModel model =
    case model.auth of
        LoggedOut error credentials ->
            viewSignInForm Login (LoggedOut error >> DataUpdated) False credentials
                :: (case error of
                        Just e ->
                            [ e ]

                        Nothing ->
                            []
                   )

        LoggingIn credentials ->
            [ viewSignInForm Login (LoggedOut Nothing >> DataUpdated) True credentials ]

        LoggedIn userInfo data ->
            [ div [] [ text "Welcome, ", displayNamedNode userUrl userInfo.node ]
            , div [] [ button [ type_ "button", onClick Logout ] [ text "Log out" ] ]
            , hr [] []
            , ul []
                [ li [] [ a [ href <| Url.Builder.absolute [ "groups" ] [] ] [ text "Groups" ] ]
                ]
            , hr [] []
            , displayData model.now userInfo data
            ]


displayData : Now -> UserInfo -> Data -> Html Msg
displayData now userInfo remoteData =
    case remoteData of
        NoData ->
            p [] [ text "Click on a link to begin" ]

        Me data ->
            displayRemoteData (MyDetails.view now) data

        GroupList data ->
            displayRemoteData displayGroupList data

        GroupDetail data ->
            displayRemoteData displayGroupDetail data

        UserDetail data ->
            displayRemoteData displayUserDetail data

        UserScore data ->
            displayRemoteData (UserScore.view (Success >> UserScore >> LoggedIn userInfo >> DataUpdated) now) data


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
    dl []
        [ dt [] [ text <| "Groups: " ++ (String.fromInt <| List.length groups) ]
        , dd [] [ ul [] <| List.map (displayNamedNode groupUrl >> List.singleton >> li []) groups ]
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
