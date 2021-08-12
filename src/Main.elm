module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http exposing (HttpError(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, toTime, viewData)
import Html exposing (Html, a, b, dd, div, dl, dt, h4, li, p, strong, text, ul)
import Html.Attributes exposing (href)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Object.Case as Case
import Predictions.Object.Diagnosis as Diagnosis
import Predictions.Object.Group as Group
import Predictions.Object.Judgement as Judgement
import Predictions.Object.User as User
import Predictions.Object.Wager as Wager
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)



-- MAIN


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


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }



-- MODEL


type PossibleData
    = UserList (GraphqlRemoteData UserListResponse)
    | UserData (GraphqlRemoteData UserDetailResponse)
    | GroupList (GraphqlRemoteData GroupListResponse)
    | GroupData (GraphqlRemoteData GroupDetailResponse)
    | CaseData (GraphqlRemoteData CaseDetailResponse)
    | NoData


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , data : PossibleData
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url NoData, parseUrlAndRequest url )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResponse PossibleData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/graphql" then
                        ( model, Nav.load (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , parseUrlAndRequest url
            )

        GotResponse graphqlRemoteData ->
            ( { model | data = graphqlRemoteData }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Predictions"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ li [] [ a [ href "/graphql" ] [ text "GraphiQL" ] ]
            , viewLink "/users"
            , viewLink "/groups"
            ]
        , displayData model.data
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


displayMaybe : (a -> Html msg) -> Maybe a -> Html msg
displayMaybe displayFunction data =
    case data of
        Just a ->
            displayFunction a

        Nothing ->
            div [] [ text "Not found" ]


displayData : PossibleData -> Html msg
displayData possibleData =
    case possibleData of
        NoData ->
            text "No data!"

        UserList graphqlRemoteData ->
            viewData (displayNamedNodeList "/user") graphqlRemoteData

        UserData graphqlRemoteData ->
            viewData (displayMaybe displayUser) graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group") graphqlRemoteData

        GroupData graphqlRemoteData ->
            viewData (displayMaybe displayGroup) graphqlRemoteData

        CaseData graphqlRemoteData ->
            viewData (displayMaybe displayCase) graphqlRemoteData


displayNamedNodeLink : String -> NamedNodeData -> Html msg
displayNamedNodeLink base_path node =
    case node.id of
        Id id ->
            a [ href <| base_path ++ "/" ++ id ] [ text node.name ]


displayNamedNodeList : String -> List NamedNodeData -> Html msg
displayNamedNodeList base_path nodes =
    displayInList (displayNamedNodeLink base_path >> List.singleton) nodes


displayInList : (a -> List (Html msg)) -> List a -> Html msg
displayInList displayFunction list =
    ul [] <| List.map (li [] << displayFunction) list


displayUser : UserDetailData -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/user" user.node ]
        , dt [] [ text "Created" ]
        , dd [] [ text <| toTime user.created ]
        , dt [] [ text <| "Groups (" ++ String.fromInt (List.length user.groups) ++ ")" ]
        , dd [] [ displayNamedNodeList "/group" user.groups ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length user.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case" user.cases ]
        ]


displayGroup : GroupDetailData -> Html msg
displayGroup group =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/group" group.node ]
        , dt [] [ text <| "Members (" ++ String.fromInt (List.length group.members) ++ ")" ]
        , dd [] [ displayNamedNodeList "/user" group.members ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length group.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case" group.cases ]
        ]


displayCase : CaseDetailData -> Html msg
displayCase case_ =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNodeLink "/case" case_.node ]
        , dt [] [ text "Deadline" ]
        , dd [] [ text <| toTime case_.deadline ]
        , dt [] [ text "Creator" ]
        , dd [] [ displayNamedNodeLink "/user" case_.creator ]
        , dt [] [ text "Group" ]
        , dd []
            [ case case_.group of
                Just group ->
                    displayNamedNodeLink "/group" group

                Nothing ->
                    text "None"
            ]
        , dt [] [ text <| "Diagnoses (" ++ String.fromInt (List.length case_.diagnoses) ++ ")" ]
        , dd [] [ displayInList displayDiagnosis case_.diagnoses ]
        ]


displayDiagnosis : DiagnosisDetailData -> List (Html msg)
displayDiagnosis diagnosis =
    [ h4 [] [ text diagnosis.node.name ]
    , displayInList displayWager diagnosis.wagers
    , displayJudgement diagnosis.judgement
    ]


displayWager : WagerData -> List (Html msg)
displayWager wager =
    [ displayNamedNodeLink "/user" wager.creator
    , text " estimated "
    , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
    , text <| " at " ++ toTime wager.timestamp
    ]


displayJudgement : Maybe JudgementData -> Html msg
displayJudgement judgement =
    case judgement of
        Just judged ->
            p []
                [ text " Judged as "
                , strong [] [ displayOutcome judged.outcome ]
                , text " by "
                , displayNamedNodeLink "/user" judged.judgedBy
                , text <| " at " ++ toTime judged.timestamp ++ ")"
                ]

        Nothing ->
            text "Not yet judged!"


displayOutcome : Outcome -> Html msg
displayOutcome outcome =
    text
        (case outcome of
            Right ->
                "Right"

            Wrong ->
                "Wrong"

            Indeterminate ->
                "Indeterminate"
        )



-- Parse URL


type Route
    = Users
      --| Blog Int
    | User String
    | Groups
    | Group String
    | Case String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Users <| s "users"
        , map User <| s "user" </> string
        , map Groups <| s "groups"
        , map Group <| s "group" </> string
        , map Case <| s "case" </> string
        ]


parseUrlAndRequest : Url.Url -> Cmd Msg
parseUrlAndRequest url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                Users ->
                    userListQuery |> makeRequest (UserList >> GotResponse)

                User string ->
                    Id string |> userDetailQuery |> makeRequest (UserData >> GotResponse)

                Groups ->
                    groupListQuery |> makeRequest (GroupList >> GotResponse)

                Group string ->
                    Id string |> groupDetailQuery |> makeRequest (GroupData >> GotResponse)

                Case string ->
                    Id string |> caseDetailQuery |> makeRequest (CaseData >> GotResponse)

        Nothing ->
            Cmd.none



-- Fetch


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserDetailData =
    { node : NamedNodeData
    , created : Timestamp
    , groups : List NamedNodeData
    , cases : List CaseLimitedData
    }


mapToUserDetailData =
    SelectionSet.map4 UserDetailData
        (SelectionSet.map2 NamedNodeData User.id User.name)
        User.created
        (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        (User.cases <| SelectionSet.map2 NamedNodeData Case.id Case.reference)


type alias UserDetailResponse =
    Maybe UserDetailData


userDetailQuery : Id -> SelectionSet UserDetailResponse RootQuery
userDetailQuery id =
    Query.user { id = id } <| mapToUserDetailData


type alias UserListResponse =
    List NamedNodeData


userListQuery : SelectionSet UserListResponse RootQuery
userListQuery =
    Query.users <| SelectionSet.map2 NamedNodeData User.id User.name


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    , cases : List CaseLimitedData
    }


mapToGroupDetailData =
    SelectionSet.map3 GroupDetailData
        (SelectionSet.map2 NamedNodeData Group.id Group.name)
        (Group.members <| SelectionSet.map2 NamedNodeData User.id User.name)
        (Group.cases <| SelectionSet.map2 NamedNodeData Case.id Case.reference)


type alias GroupDetailResponse =
    Maybe GroupDetailData


groupDetailQuery : Id -> SelectionSet GroupDetailResponse RootQuery
groupDetailQuery id =
    Query.group { id = id } <| mapToGroupDetailData


type alias GroupListResponse =
    List NamedNodeData


groupListQuery : SelectionSet GroupListResponse RootQuery
groupListQuery =
    Query.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name


type alias CaseLimitedData =
    NamedNodeData


type alias CaseDetailData =
    { node : NamedNodeData
    , creator : NamedNodeData
    , group : Maybe NamedNodeData
    , deadline : Timestamp
    , diagnoses : List DiagnosisDetailData
    }


mapToCaseDetailData =
    SelectionSet.map5 CaseDetailData
        (SelectionSet.map2 NamedNodeData Case.id Case.reference)
        (Case.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        (Case.group <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        Case.deadline
        (Case.diagnoses <| mapToDiagnosisDetailData)


type alias CaseDetailResponse =
    Maybe CaseDetailData


caseDetailQuery : Id -> SelectionSet CaseDetailResponse RootQuery
caseDetailQuery id =
    Query.case_ { id = id } <| mapToCaseDetailData


type alias DiagnosisLimitedData =
    NamedNodeData


type alias DiagnosisDetailData =
    { node : NamedNodeData
    , wagers : List WagerData
    , judgement : Maybe JudgementData
    }


type alias WagerData =
    { creator : NamedNodeData
    , confidence : Int
    , timestamp : Timestamp
    }


type alias JudgementData =
    { judgedBy : NamedNodeData
    , timestamp : Timestamp
    , outcome : Outcome
    }


mapToDiagnosisDetailData =
    SelectionSet.map3 DiagnosisDetailData
        (SelectionSet.map2 NamedNodeData Diagnosis.id Diagnosis.name)
        (Diagnosis.wagers <| mapToWagerData)
        (Diagnosis.judgement <| mapToJudgementData)


mapToWagerData =
    SelectionSet.map3 WagerData
        (Wager.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        Wager.confidence
        Wager.timestamp


mapToJudgementData =
    SelectionSet.map3 JudgementData
        (Judgement.judgedBy <| SelectionSet.map2 NamedNodeData User.id User.name)
        Judgement.timestamp
        Judgement.outcome



-- Helper for fetch


makeRequest : (GraphqlRemoteData decodesTo -> Msg) -> SelectionSet decodesTo RootQuery -> Cmd Msg
makeRequest msgConstructor set =
    set
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> msgConstructor)
