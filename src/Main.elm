port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import CaseDetail
import CaseList
import Common exposing (HtmlRemoteData, NamedNodeData, Now, UserInfo, displayNamedNode, groupUrl, userUrl)
import Config exposing (api)
import Error
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, dd, div, dl, dt, hr, li, text, ul)
import Html.Attributes exposing (href, type_)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Resolver)
import Import exposing (DecryptionMessage)
import Json.Decode as D
import Json.Encode as E
import Login exposing (Credentials, blankCredentials, viewSignInForm)
import MyDetails
import Predictions.Enum.Outcome exposing (Outcome)
import Predictions.Object.Group
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import ScalarCodecs exposing (Timestamp)
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


port decrypt : DecryptionMessage -> Cmd msg


port decryptReceiver : (D.Value -> msg) -> Sub msg


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    parseUrlAndRequest url <| Model key (Now Time.utc <| Time.millisToPosix 0) (LoggedOut Nothing blankCredentials)


subscriptions : Model -> Sub Msg
subscriptions _ =
    decryptReceiver DecryptedReferences


type alias Model =
    { key : Nav.Key
    , now : Now
    , auth : Auth
    }


type Auth
    = LoggedOut (Maybe (Html Msg)) Credentials
    | LoggingIn Credentials
    | LoggedIn UserInfo Data
    | RefreshingToken


type Data
    = WelcomeUser
    | Me (HtmlRemoteData MyDetails.Data)
    | GroupList (HtmlRemoteData GroupListData)
    | GroupDetail (HtmlRemoteData GroupDetailData)
    | CaseList (HtmlRemoteData CaseList.Data)
    | CaseDetail (HtmlRemoteData CaseDetail.Data)
    | UserDetail (HtmlRemoteData NamedNodeData)
    | UserScore (HtmlRemoteData UserScore.Data)
    | ImportPredictions (HtmlRemoteData Import.Data)


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
    | Login Credentials
    | Logout (Maybe (Html Msg)) (Maybe Credentials)
    | LoggedInOK UserInfo
    | ExpiredToken (UserInfo -> Cmd Msg) UserInfo
    | RenewedToken (Cmd Msg)
    | DataUpdated UserInfo Data
    | RequestNewList UserInfo CaseList.Data
    | FetchGroups UserInfo CaseDetail.Data
    | ChangeGroup UserInfo CaseDetail.Data (Maybe Id)
    | ChangeDeadline UserInfo CaseDetail.Data Timestamp
    | SubmitDiagnosis UserInfo CaseDetail.Data String Int
    | SubmitWager UserInfo CaseDetail.Data Id Int
    | JudgeOutcome UserInfo CaseDetail.Data Id Outcome
    | AddComment UserInfo CaseDetail.Data String
    | RetrieveRemoteCases UserInfo Import.Data
    | GotFromApi UserInfo Import.Data
    | DecryptedReferences D.Value
    | SubmitImportedCases UserInfo Import.Data (Graphql.Http.Request Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        caseDetailHelper : UserInfo -> CaseDetail.Data -> (CaseDetail.Data -> Result (Graphql.Http.Error ()) a -> CaseDetail.Data) -> Graphql.Http.Request a -> ( Model, Cmd Msg )
        caseDetailHelper userInfo data onResult request =
            ( { model | auth = LoggedIn userInfo <| CaseDetail <| Success data }
            , sendRequest
                request
                (onResult data >> Success >> CaseDetail)
                userInfo
            )
    in
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

        Login credentials ->
            let
                resultToMsg : Result (Html Msg) UserInfo -> Msg
                resultToMsg result =
                    case result of
                        Ok user ->
                            LoggedInOK user

                        Err error ->
                            Logout (Just error) (Just credentials)
            in
            ( { model | auth = LoggingIn credentials }
            , Login.login resultToMsg credentials
            )

        Logout err credentials ->
            ( { model | auth = LoggedOut err <| Maybe.withDefault blankCredentials credentials }
            , Cmd.none
            )

        LoggedInOK user ->
            ( { model | auth = LoggedIn user WelcomeUser }
              --, Nav.pushUrl model.key <| userUrl user.id
            , Cmd.none
            )

        ExpiredToken sendNewRequest originalUser ->
            let
                toMsg : Result Graphql.Http.HttpError UserInfo -> Msg
                toMsg result =
                    case result of
                        Ok newUser ->
                            RenewedToken (sendNewRequest newUser)

                        Err error ->
                            Logout (Just <| Error.httpErrorToHtml error) Nothing

                decoder : D.Decoder UserInfo
                decoder =
                    D.map2
                        (UserInfo originalUser.node)
                        (D.field "accessToken" D.string)
                        (D.field "refreshToken" D.string)

                toResult : Http.Response String -> Result Graphql.Http.HttpError UserInfo
                toResult response =
                    case response of
                        Http.BadUrl_ url ->
                            Err (Graphql.Http.BadUrl url)

                        Http.Timeout_ ->
                            Err Graphql.Http.Timeout

                        Http.NetworkError_ ->
                            Err Graphql.Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (Graphql.Http.BadStatus metadata body)

                        Http.GoodStatus_ _ body ->
                            case D.decodeString decoder body of
                                Ok newUser ->
                                    Ok newUser

                                Err err ->
                                    Err (Graphql.Http.BadPayload err)
            in
            ( { model | auth = RefreshingToken }
            , Http.post
                { url = Url.Builder.absolute [ "auth", "refresh" ] []
                , body = Http.jsonBody <| E.object [ ( "token", E.string originalUser.refreshToken ) ]
                , expect = Http.expectStringResponse toMsg toResult
                }
            )

        RenewedToken cmd ->
            ( model, cmd )

        DataUpdated userInfo data ->
            ( { model | auth = LoggedIn userInfo data }, Task.map2 Now Time.here Time.now |> Task.perform NewTime )

        RequestNewList userInfo data ->
            CaseList.queryRequest data
                |> sendDataRequest model userInfo CaseList

        FetchGroups userInfo data ->
            CaseDetail.fetchGroups userInfo.node.id
                |> caseDetailHelper userInfo data CaseDetail.onFetchGroupsResult

        ChangeGroup userInfo data group ->
            CaseDetail.changeGroup data group
                |> caseDetailHelper userInfo data CaseDetail.onChangeGroupResult

        ChangeDeadline userInfo data deadline ->
            CaseDetail.changeDeadline data deadline
                |> caseDetailHelper userInfo data CaseDetail.onChangeDeadlineResult

        SubmitDiagnosis userInfo data diagnosis confidence ->
            CaseDetail.submitDiagnosis data diagnosis confidence
                |> caseDetailHelper userInfo data CaseDetail.onSubmitDiagnosisResult

        SubmitWager userInfo data diagnosisId confidence ->
            CaseDetail.submitWager diagnosisId confidence
                |> caseDetailHelper userInfo data (CaseDetail.onSubmitWagerResult diagnosisId)

        JudgeOutcome userInfo data diagnosisId outcome ->
            CaseDetail.judgeOutcome diagnosisId outcome
                |> caseDetailHelper userInfo data (CaseDetail.onJudgeOutcomeResult diagnosisId)

        AddComment userInfo data comment ->
            CaseDetail.addComment data comment
                |> caseDetailHelper userInfo data CaseDetail.onAddCommentResult

        RetrieveRemoteCases userInfo data ->
            ( model, Import.queryRequest (GotFromApi userInfo) data )

        GotFromApi userInfo data ->
            ( { model | auth = Success data |> ImportPredictions |> LoggedIn userInfo }, data |> Import.toDecryptionCmd decrypt )

        DecryptedReferences value ->
            ( case model.auth of
                LoggedIn userInfo (ImportPredictions (Success data)) ->
                    { model | auth = data |> Import.updateWithDecrypted value |> Success |> ImportPredictions |> LoggedIn userInfo }

                _ ->
                    model
            , Cmd.none
            )

        SubmitImportedCases userInfo data request ->
            request |> sendDataRequest model userInfo (Import.onSubmitCasesResult data >> Success >> ImportPredictions)


type Route
    = Home
    | UserDetailRoute Id
    | UserScoreRoute Id
    | GroupsRoute
    | GroupDetailRoute Id
    | CasesRoute
    | CaseDetailRoute Id
    | ImportRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map GroupsRoute <| s "groups"
        , map (GroupDetailRoute << Id) <| s "groups" </> string
        , map CasesRoute <| s "cases"
        , map (CaseDetailRoute << Id) <| s "cases" </> string
        , map (UserDetailRoute << Id) <| s "users" </> string
        , map (UserScoreRoute << Id) <| s "users" </> string </> s "score"
        , map ImportRoute <| s "import"
        ]


parseUrlAndRequest : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrlAndRequest url model =
    case ( model.auth, Url.Parser.parse routeParser url ) of
        ( LoggedIn userData _, Just Home ) ->
            ( { model | auth = LoggedIn userData WelcomeUser }, Cmd.none )

        ( _, Just Home ) ->
            ( model, Cmd.none )

        ( LoggedIn userData _, Just (UserDetailRoute userId) ) ->
            if userId == userData.node.id then
                MyDetails.queryRequest userId
                    |> sendDataRequest model userData Me

            else
                SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                    |> Predictions.Query.user { id = userId }
                    |> Graphql.Http.queryRequest api
                    |> sendDataRequest model userData UserDetail

        ( LoggedIn userData _, Just GroupsRoute ) ->
            SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name
                |> Predictions.Object.User.groups
                |> Predictions.Query.user { id = userData.node.id }
                |> Graphql.Http.queryRequest api
                |> sendDataRequest model userData GroupList

        ( LoggedIn userData _, Just (GroupDetailRoute groupId) ) ->
            SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name
                |> Predictions.Object.Group.members
                |> SelectionSet.map2 GroupDetailData (SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
                |> Predictions.Query.group { id = groupId }
                |> Graphql.Http.queryRequest api
                |> sendDataRequest model userData GroupDetail

        ( LoggedIn userData _, Just CasesRoute ) ->
            CaseList.initialQuery userData.node.id
                |> sendDataRequest model userData CaseList

        ( LoggedIn userData _, Just (CaseDetailRoute caseId) ) ->
            CaseDetail.queryRequest caseId
                |> sendDataRequest model userData CaseDetail

        ( LoggedIn userData _, Just (UserScoreRoute userId) ) ->
            UserScore.queryRequest userId
                |> sendDataRequest model userData UserScore

        ( LoggedIn userData _, Just ImportRoute ) ->
            Import.fetchGroups userData.node.id
                |> sendDataRequest model userData (RemoteData.map Import.initialData >> ImportPredictions)

        _ ->
            ( model, Nav.pushUrl model.key "/" )


sendRequest : Graphql.Http.Request a -> (Result (Graphql.Http.Error ()) a -> Data) -> UserInfo -> Cmd Msg
sendRequest request resultToData user =
    let
        handleExpiredToken : Result (Graphql.Http.Error ()) a -> Msg
        handleExpiredToken result =
            case result of
                Err (Graphql.Http.HttpError (Graphql.Http.BadStatus _ apiBody)) ->
                    case D.decodeString (D.field "error" D.string) apiBody of
                        Ok "jwt expired" ->
                            ExpiredToken (sendRequest request resultToData) user

                        _ ->
                            DataUpdated user (resultToData result)

                _ ->
                    DataUpdated user (resultToData result)
    in
    request
        |> Graphql.Http.withHeader "Authorization" ("Bearer " ++ user.accessToken)
        |> Graphql.Http.send (Graphql.Http.discardParsedErrorData >> handleExpiredToken)


sendDataRequest : Model -> UserInfo -> (HtmlRemoteData a -> Data) -> Graphql.Http.Request a -> ( Model, Cmd Msg )
sendDataRequest model user toData request =
    ( { model | auth = LoggedIn user (toData Loading) }
    , sendRequest request (RemoteData.fromResult >> toData) user
    )


view : Model -> Document Msg
view model =
    { title = "Predictions"
    , body = viewModel model
    }


viewModel : Model -> List (Html Msg)
viewModel model =
    case model.auth of
        LoggedOut error credentials ->
            viewSignInForm Login (Just >> Logout error) False credentials
                :: (case error of
                        Just e ->
                            [ e ]

                        Nothing ->
                            []
                   )

        LoggingIn credentials ->
            [ viewSignInForm Login (Just >> Logout Nothing) True credentials ]

        LoggedIn userInfo data ->
            [ div []
                [ text "Logged in as: "
                , displayNamedNode userUrl userInfo.node
                , button [ type_ "button", onClick <| Logout Nothing Nothing ] [ text "Log out" ]
                ]
            , hr [] []
            , ul []
                [ li [] [ a [ href <| Url.Builder.absolute [ "groups" ] [] ] [ text "Groups" ] ]
                , li [] [ a [ href <| Url.Builder.absolute [ "cases" ] [] ] [ text "Cases" ] ]
                , li [] [ a [ href <| Url.Builder.absolute [ "import" ] [] ] [ text "Import" ] ]
                ]
            , hr [] []
            , displayData model.now userInfo data
            ]

        RefreshingToken ->
            [ text "Refreshing token..." ]


displayData : Now -> UserInfo -> Data -> Html Msg
displayData now userInfo remoteData =
    case remoteData of
        WelcomeUser ->
            text "Let's begin!"

        Me data ->
            MyDetails.view now |> displayRemoteData data

        UserDetail data ->
            displayUserDetail |> displayRemoteData data

        GroupList data ->
            displayGroupList |> displayRemoteData data

        GroupDetail data ->
            displayGroupDetail |> displayRemoteData data

        CaseList data ->
            CaseList.view (RequestNewList userInfo) |> displayRemoteData data

        CaseDetail data ->
            CaseDetail.view
                (Success >> CaseDetail >> DataUpdated userInfo)
                (FetchGroups userInfo)
                (ChangeGroup userInfo)
                (ChangeDeadline userInfo)
                (SubmitDiagnosis userInfo)
                (SubmitWager userInfo)
                (JudgeOutcome userInfo)
                (AddComment userInfo)
                now
                userInfo.node
                |> displayRemoteData data

        UserScore data ->
            UserScore.view (Success >> UserScore >> DataUpdated userInfo) now |> displayRemoteData data

        ImportPredictions data ->
            Import.view
                (RetrieveRemoteCases userInfo)
                (Success >> ImportPredictions >> DataUpdated userInfo)
                (SubmitImportedCases userInfo)
                now
                |> displayRemoteData data


displayRemoteData : HtmlRemoteData a -> (a -> Html Msg) -> Html Msg
displayRemoteData remoteData displayFunction =
    case remoteData of
        NotAsked ->
            text "Not asked"

        Loading ->
            text "Loading..."

        Failure err ->
            Error.graphqlHttpErrorToHtml err

        Success data ->
            displayFunction data


displayGroupList : GroupListData -> Html Msg
displayGroupList groups =
    dl []
        [ dt [] [ text <| "Groups (" ++ (String.fromInt <| List.length groups) ++ ")" ]
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
