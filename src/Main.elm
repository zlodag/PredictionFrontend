module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Error
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (Now, displayOutcomeSymbol, displayTime, getShortDateString)
import Html exposing (Html, a, button, dd, div, dl, dt, hr, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (href, id, style, type_, value)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Resolver)
import Json.Decode as D
import Json.Encode as E
import Login exposing (Credentials, blankCredentials, displaySignInForm, encodeCredentials)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Object.Group
import Predictions.Object.Score
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import Round
import ScalarCodecs exposing (Timestamp)
import Svg as S
import Task
import Time
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


type alias UserInfo =
    { node : NamedNodeData
    , accessToken : String
    , refreshToken : String
    }


type Data
    = NoData
    | Me (HtmlRemoteData MyDetails)
    | GroupList (HtmlRemoteData GroupListData)
    | GroupDetail (HtmlRemoteData GroupDetailData)
    | UserDetail (HtmlRemoteData NamedNodeData)
    | UserScore (List (CI.One Score CI.Dot)) (HtmlRemoteData (List Score))


type alias HtmlRemoteData a =
    RemoteData (Html Msg) a


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias MyDetails =
    { node : NamedNodeData
    , created : Timestamp
    , score : Maybe Float
    }


type alias GroupListData =
    List NamedNodeData


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    }


type alias Score =
    { judged : Timestamp
    , case_ : NamedNodeData
    , diagnosis : String
    , confidence : Int
    , outcome : Outcome
    , brierScore : Float
    , averageBrierScore : Float
    , adjustedBrierScore : Float
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
                userDecoder : D.Decoder UserInfo
                userDecoder =
                    D.map3
                        UserInfo
                        (D.field "user" <| D.map2 NamedNodeData (D.field "id" <| D.map Id D.string) (D.field "name" D.string))
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
                    case result of
                        Ok user ->
                            LoggedInOK user

                        Err error ->
                            LoggedOut (Just error) credentials |> DataUpdated
            in
            ( { model | auth = LoggingIn credentials }
            , Http.post
                { url = Url.Builder.absolute [ "auth", "login" ] []
                , body = Http.jsonBody <| encodeCredentials credentials
                , expect = Http.expectStringResponse resultToMsg responseToResult
                }
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
                , SelectionSet.map3 MyDetails
                    (SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
                    Predictions.Object.User.created
                    (Predictions.Object.User.score { adjusted = False })
                    |> Predictions.Query.user { id = userId }
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
            ( { model | auth = LoggedIn userData (UserScore [] Loading) }
            , SelectionSet.map8 Score
                Predictions.Object.Score.judged
                (SelectionSet.map2 NamedNodeData Predictions.Object.Score.caseId Predictions.Object.Score.reference)
                Predictions.Object.Score.diagnosis
                Predictions.Object.Score.confidence
                Predictions.Object.Score.outcome
                Predictions.Object.Score.brierScore
                Predictions.Object.Score.averageBrierScore
                Predictions.Object.Score.adjustedBrierScore
                |> Predictions.Object.User.scores
                |> Predictions.Query.user { id = userId }
                |> sendRequest (UserScore []) userData
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
            displaySignInForm Login (LoggedOut error >> DataUpdated) False credentials
                :: (case error of
                        Just e ->
                            [ e ]

                        Nothing ->
                            []
                   )

        LoggingIn credentials ->
            [ displaySignInForm Login (LoggedOut Nothing >> DataUpdated) True credentials ]

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
            displayRemoteData (displayMyDetails now) data

        GroupList data ->
            displayRemoteData displayGroupList data

        GroupDetail data ->
            displayRemoteData displayGroupDetail data

        UserDetail data ->
            displayRemoteData displayUserDetail data

        UserScore hovering data ->
            displayRemoteData (displayUserScore now userInfo hovering) data


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


displayMyDetails : Now -> MyDetails -> Html Msg
displayMyDetails now myDetails =
    dl []
        [ dt [] [ text "Me" ]
        , dd [] [ displayNamedNode userUrl myDetails.node ]
        , dt [] [ text "Created" ]
        , dd [] [ displayTime now myDetails.created ]
        , dt [] [ text "Score" ]
        , dd []
            [ case myDetails.score of
                Just score ->
                    a
                        [ href <|
                            Url.Builder.absolute
                                [ "user"
                                , case myDetails.node.id of
                                    Id id ->
                                        id
                                , "score"
                                ]
                                []
                        ]
                        [ text <| Round.round 4 score ]

                Nothing ->
                    text "No predictions judged yet"
            ]
        ]


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


displayUserScore : Now -> UserInfo -> List (CI.One Score CI.Dot) -> List Score -> Html Msg
displayUserScore now userInfo hovering scores =
    let
        onHover : List (CI.One Score CI.Dot) -> Msg
        onHover newHovering =
            Success scores |> UserScore newHovering >> LoggedIn userInfo >> DataUpdated
    in
    div []
        [ div [ style "width" "500px" ] [ displayChart now.zone onHover hovering scores ]
        , div [ style "width" "500px" ] [ displayConfidenceHistogram scores ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Judged" ]
                    , th [] [ text "Case" ]
                    , th [] [ text "Diagnosis" ]
                    , th [] [ text "Confidence" ]
                    , th [] [ text "Outcome" ]
                    , th [] [ text "Brier score" ]
                    , th [] [ text "Running average Brier score" ]
                    , th [] [ text "Running adjusted Brier score" ]
                    ]
                ]
            , tbody [] <| List.map (displayScore now) scores
            ]
        ]


displayScore : Now -> Score -> Html msg
displayScore now score =
    tr []
        [ td [] [ displayTime now score.judged ]
        , td [] [ displayNamedNode caseUrl score.case_ ]
        , td [] [ text score.diagnosis ]
        , td [] [ text <| String.fromInt score.confidence ++ "%" ]
        , td [] [ text <| displayOutcomeSymbol score.outcome ]
        , td [] [ text <| Round.round 4 score.brierScore ]
        , td [] [ text <| Round.round 4 score.averageBrierScore ]
        , td [] [ text <| Round.round 4 score.adjustedBrierScore ]
        ]


displayChart : Time.Zone -> (List (CI.One Score CI.Dot) -> Msg) -> List (CI.One Score CI.Dot) -> List Score -> Html Msg
displayChart zone onHover hovering scores =
    C.chart
        [ CA.width 400
        , CA.height 200
        , CA.margin { top = 50, bottom = 50, left = 50, right = 50 }
        , CE.onMouseMove onHover (CI.named [ "Brier score" ] |> CI.andThen CI.dots |> CE.getNearest)
        , CE.onMouseLeave (onHover [])
        , CA.domain
            [ CA.lowest 0 CA.exactly
            , CA.highest 1 CA.exactly
            ]
        ]
        [ C.xTicks [ CA.times zone ]
        , C.yTicks []
        , C.xLabels
            [ CA.times zone
            , CA.format (floor >> Time.millisToPosix >> getShortDateString zone)
            , CA.fontSize 12
            ]
        , C.yLabels [ CA.fontSize 12 ]
        , C.series (.judged >> Time.posixToMillis >> toFloat)
            [ C.scatter .brierScore [] |> C.named "Brier score"
            , C.interpolated .averageBrierScore [ CA.stepped ] [] |> C.named "average score"

            --, C.interpolated .adjustedBrierScore [ CA.stepped ] [] |> C.named "adjusted score"
            ]
            scores
        , C.legendsAt .max .max [ CA.alignRight, CA.moveUp 30 ] []
        , C.each hovering <|
            \_ item ->
                [ C.tooltip item [] [] [] ]
        ]


displayConfidenceHistogram : List Score -> Html Msg
displayConfidenceHistogram scores =
    let
        filter : Int -> Score -> Bool
        filter lowerBound score =
            let
                foldedConfidence =
                    50 + abs (score.confidence - 50)
            in
            (if lowerBound == 50 then
                (>=)

             else
                (>)
            )
                foldedConfidence
                lowerBound
                && foldedConfidence
                <= (lowerBound + 5)

        mapFn lowerBound =
            List.filter (filter lowerBound) scores |> foldl lowerBound

        foldFn score state =
            if score.outcome == Right then
                { state | correct = state.correct + 1 }

            else
                state

        foldl lowerBound filteredScores =
            List.foldl foldFn { lowerBound = lowerBound, total = List.length filteredScores, correct = 0 } filteredScores

        data =
            List.range 0 9 |> List.map (\x -> 50 + x * 5) |> List.map mapFn

        scoreGetY : { bin | total : Int, correct : Int } -> Float
        scoreGetY bin =
            if bin.total == 0 then
                0

            else
                toFloat bin.correct * 100 / toFloat bin.total
    in
    C.chart
        [ CA.width 400
        , CA.height 200
        , CA.margin { top = 50, bottom = 50, left = 50, right = 50 }
        , CA.range
            [ CA.lowest 50 CA.exactly
            , CA.highest 100 CA.exactly
            ]
        , CA.domain
            [ CA.lowest 0 CA.exactly
            , CA.highest 100 CA.exactly
            ]
        ]
        [ C.xLabels
            [ CA.fontSize 12
            , CA.amount 11
            , CA.limits
                [ CA.lowest 50 CA.exactly
                , CA.highest 100 CA.exactly
                ]
            ]
        , C.yLabels [ CA.withGrid, CA.fontSize 12 ]
        , C.bars
            [ CA.x1 (.lowerBound >> toFloat)
            , CA.margin 0.02
            ]
            [ C.bar scoreGetY [] |> C.named "Accuracy (%)" ]
            data
        , C.line
            [ CA.x1 50
            , CA.y1 50
            , CA.x2 100
            , CA.y2 100
            ]
        , C.labelAt (CA.percent 50) (CA.percent 0) [ CA.moveDown 35, CA.fontSize 12 ] [ S.text "Confidence (%)" ]
        , C.legendsAt .max .max [ CA.alignRight, CA.moveUp 30 ] []
        ]


displayNamedNode : (Id -> String) -> NamedNodeData -> Html msg
displayNamedNode toUrl node =
    a [ node.id |> toUrl |> href ] [ text node.name ]


userUrl : Id -> String
userUrl (Id id) =
    Url.Builder.absolute [ "users", id ] []


groupUrl : Id -> String
groupUrl (Id id) =
    Url.Builder.absolute [ "groups", id ] []


caseUrl : Id -> String
caseUrl (Id id) =
    Url.Builder.absolute [ "cases", id ] []
