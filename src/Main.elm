module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Graphql.Http exposing (Error, HttpError(..), Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, viewData)
import Html exposing (Html, a, b, button, dd, div, dl, dt, fieldset, h4, input, li, option, p, select, span, strong, text, ul)
import Html.Attributes exposing (href, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601 exposing (fromTime)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject
import Predictions.Mutation as Mutation
import Predictions.Object.Case as Case
import Predictions.Object.Diagnosis as Diagnosis
import Predictions.Object.Group as Group
import Predictions.Object.Judgement as Judgement
import Predictions.Object.User as User
import Predictions.Object.Wager as Wager
import Predictions.Query as Query exposing (GroupsOptionalArguments)
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import ScalarCodecs exposing (Id, Timestamp)
import String
import Time
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



-- MODEL


type alias PredictionInput =
    { diagnosis : FormField String
    , confidence : FormField Int
    }


type Validity
    = Valid
    | Invalid String


type alias FormField a =
    { value : a
    , validity : Validity
    }


type alias CaseData =
    { creatorId : Id
    , userGroups : GraphqlRemoteData GroupListResponse
    , groupId : Maybe Id
    , reference : FormField String
    , predictions : List PredictionInput
    , deadline : FormField Timestamp
    }


type State
    = UserList (GraphqlRemoteData UserListResponse)
    | UserDetail (GraphqlRemoteData UserDetailResponse)
    | GroupList (GraphqlRemoteData GroupListResponse)
    | GroupDetail (GraphqlRemoteData GroupDetailResponse)
    | CaseDetail (GraphqlRemoteData CaseDetailResponse)
    | NewCase CaseData
    | NoData


type alias Model =
    { key : Nav.Key
    , state : State
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    parseUrlAndRequest (Model key NoData) url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResponse State
    | CaseChanged CaseData
    | SubmitCase CaseData
    | CaseCreated (GraphqlRemoteData Id)



--| GotUserGroups Id (GraphqlRemoteData GroupListResponse)


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
            parseUrlAndRequest model url

        GotResponse graphqlRemoteData ->
            ( { model | state = graphqlRemoteData }
            , Cmd.none
            )

        CaseChanged caseInput ->
            ( { model | state = NewCase caseInput }
            , Cmd.none
            )

        CaseCreated graphqlRemoteData ->
            case graphqlRemoteData of
                Success (Id id) ->
                    ( model, Nav.pushUrl model.key <| "/case/" ++ id )

                _ ->
                    ( model, Cmd.none )

        SubmitCase caseInput ->
            ( model, submitCase caseInput )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Predictions"
    , body =
        [ ul []
            [ li [] [ a [ href "/graphql" ] [ text "GraphiQL" ] ]
            , viewLink "/users/"
            , viewLink "/groups/"
            ]
        , displayData model.state
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


displayData : State -> Html Msg
displayData possibleData =
    case possibleData of
        NoData ->
            text "No data!"

        UserList graphqlRemoteData ->
            viewData (displayNamedNodeList "/user/") graphqlRemoteData

        UserDetail graphqlRemoteData ->
            viewData (displayMaybe displayUser) graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group/") graphqlRemoteData

        GroupDetail graphqlRemoteData ->
            viewData (displayMaybe displayGroup) graphqlRemoteData

        CaseDetail graphqlRemoteData ->
            viewData (displayMaybe displayCase) graphqlRemoteData

        NewCase input ->
            displayNewForm input


displayNamedNodeLink : String -> NamedNodeData -> Html msg
displayNamedNodeLink base_path node =
    case node.id of
        Id id ->
            a [ href <| base_path ++ id ++ "/" ] [ text node.name ]


displayNamedNodeList : String -> List NamedNodeData -> Html msg
displayNamedNodeList base_path nodes =
    displayInList (displayNamedNodeLink base_path >> List.singleton) nodes


displayInList : (a -> List (Html msg)) -> List a -> Html msg
displayInList displayFunction list =
    ul [] <| List.map (li [] << displayFunction) list


displayUser : UserDetailData -> Html msg
displayUser user =
    div []
        [ dl []
            [ dt [] [ text "Name" ]
            , dd [] [ displayNamedNodeLink "/user/" user.node ]
            , dt [] [ text "Created" ]
            , dd [] [ text <| fromTime user.created ]
            , dt [] [ text <| "Groups (" ++ String.fromInt (List.length user.groups) ++ ")" ]
            , dd [] [ displayNamedNodeList "/group/" user.groups ]
            , dt [] [ text <| "Cases (" ++ String.fromInt (List.length user.cases) ++ ")" ]
            , dd [] [ displayNamedNodeList "/case/" user.cases ]
            ]
        , viewLink "new/"
        ]


displayGroup : GroupDetailData -> Html msg
displayGroup group =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/group/" group.node ]
        , dt [] [ text <| "Members (" ++ String.fromInt (List.length group.members) ++ ")" ]
        , dd [] [ displayNamedNodeList "/user/" group.members ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length group.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case/" group.cases ]
        ]


displayCase : CaseDetailData -> Html msg
displayCase case_ =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNodeLink "/case/" case_.node ]
        , dt [] [ text "Deadline" ]
        , dd [] [ text <| fromTime case_.deadline ]
        , dt [] [ text "Creator" ]
        , dd [] [ displayNamedNodeLink "/user/" case_.creator ]
        , dt [] [ text "Group" ]
        , dd []
            [ case case_.group of
                Just group ->
                    displayNamedNodeLink "/group/" group

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
    [ displayNamedNodeLink "/user/" wager.creator
    , text " estimated "
    , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
    , text <| " at " ++ fromTime wager.timestamp
    ]


displayJudgement : Maybe JudgementData -> Html msg
displayJudgement judgement =
    case judgement of
        Just judged ->
            p []
                [ text " Judged as "
                , strong [] [ displayOutcome judged.outcome ]
                , text " by "
                , displayNamedNodeLink "/user/" judged.judgedBy
                , text <| " at " ++ fromTime judged.timestamp ++ ")"
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


validateNonEmptyField : String -> String -> FormField String
validateNonEmptyField fieldName reference =
    FormField
        reference
        (if
            String.isEmpty
                reference
         then
            Invalid <| fieldName ++ " is required"

         else
            Valid
        )


caseReferenceChanged : CaseData -> String -> Msg
caseReferenceChanged caseInput reference =
    CaseChanged { caseInput | reference = validateReference reference }


validateDeadline : String -> FormField Time.Posix
validateDeadline deadline =
    case Iso8601.toTime deadline of
        Ok value ->
            FormField value Valid

        _ ->
            FormField (Time.millisToPosix 0) (Invalid "Could not parse as ISO-8601 datetime string")


caseDeadlineChanged : CaseData -> String -> Msg
caseDeadlineChanged caseInput deadline =
    CaseChanged { caseInput | deadline = validateDeadline deadline }


blankPrediction =
    PredictionInput (validateDiagnosis "") (validateConfidence "0")


addNewLine : CaseData -> Msg
addNewLine caseInput =
    CaseChanged { caseInput | predictions = caseInput.predictions ++ [ blankPrediction ] }


predictionChanged : Int -> PredictionInput -> CaseData -> Msg
predictionChanged index newPrediction caseInput =
    CaseChanged { caseInput | predictions = List.take index caseInput.predictions ++ [ newPrediction ] ++ List.drop (index + 1) caseInput.predictions }


validateDiagnosis =
    validateNonEmptyField "Diagnosis"


validateReference =
    validateNonEmptyField "Reference"


diagnosisChanged : Int -> PredictionInput -> CaseData -> String -> Msg
diagnosisChanged index oldPrediction caseInput diagnosis =
    predictionChanged index { oldPrediction | diagnosis = validateDiagnosis diagnosis } caseInput


validateConfidence : String -> FormField Int
validateConfidence confidence =
    case String.toInt confidence of
        Just a ->
            FormField a <|
                --todo change this to html validation
                if a < 0 then
                    Invalid "Minimum confidence is 0%"

                else if a > 100 then
                    Invalid "Maximum confidence is 100%"

                else
                    Valid

        Nothing ->
            FormField -1 (Invalid "Enter a number")


confidenceChanged : Int -> PredictionInput -> CaseData -> String -> Msg
confidenceChanged index oldPrediction caseInput confidence =
    predictionChanged index { oldPrediction | confidence = validateConfidence confidence } caseInput


displayPrediction : CaseData -> Int -> PredictionInput -> Html Msg
displayPrediction caseInput index prediction =
    fieldset []
        [ div []
            [ input [ placeholder "Diagnosis", value prediction.diagnosis.value, onInput (diagnosisChanged index prediction caseInput) ] []
            , displayValidity prediction.diagnosis.validity
            ]
        , div []
            [ input [ placeholder "Confidence (%)", type_ "number", value (String.fromInt prediction.confidence.value), onInput (confidenceChanged index prediction caseInput) ] []
            , displayValidity prediction.confidence.validity
            ]
        ]


printPrediction : PredictionInput -> List (Html Msg)
printPrediction prediction =
    [ text <| prediction.diagnosis.value ++ ": " ++ String.fromInt prediction.confidence.value ++ "% confidence"
    ]


displayValidity : Validity -> Html Msg
displayValidity valid =
    case valid of
        Valid ->
            span [ style "background-color" "green", style "color" "white" ] [ text "✓" ]

        Invalid string ->
            span [ style "background-color" "red", style "color" "white" ] [ text ("✗ " ++ string) ]


isValidPrediction : PredictionInput -> Bool
isValidPrediction predictionInput =
    List.all isValid [ predictionInput.diagnosis.validity, predictionInput.confidence.validity ]


isValidCase : CaseData -> Bool
isValidCase caseInput =
    isValid caseInput.reference.validity
        && isValid caseInput.deadline.validity
        && not (List.isEmpty caseInput.predictions)
        && List.all isValidPrediction caseInput.predictions


isValid : Validity -> Bool
isValid validity =
    validity == Valid


displayDebug : CaseData -> Html Msg
displayDebug caseInput =
    if isValidCase caseInput then
        dl []
            [ dt [] [ text "Reference" ]
            , dd [] [ text caseInput.reference.value ]
            , dt [] [ text "Deadline" ]
            , dd [] [ caseInput.deadline.value |> Iso8601.fromTime >> String.dropRight 5 |> text ]
            , dt [] [ text "Group" ]
            , dd []
                [ text <|
                    case caseInput.groupId of
                        Just value ->
                            case value of
                                Id id ->
                                    id

                        Nothing ->
                            "None"
                ]
            , dt [] [ text "Predictions" ]
            , dd [] [ caseInput.predictions |> List.map (printPrediction >> li []) |> ul [] ]
            , button [ onClick <| SubmitCase caseInput ] [ text "Submit" ]
            ]

    else
        text "..."


displayOption : (NamedNodeData -> Bool) -> NamedNodeData -> Html Msg
displayOption isSelected node =
    case node.id of
        Id id ->
            option [ value id, selected <| isSelected node ] [ text node.name ]


caseGroupChanged : CaseData -> String -> Msg
caseGroupChanged caseInput groupId =
    CaseChanged
        { caseInput
            | groupId =
                if String.isEmpty groupId then
                    Nothing

                else
                    Just <| Id groupId
        }


determineSelected : Maybe Id -> NamedNodeData -> Bool
determineSelected selectedId node =
    selectedId == Just node.id


displayGroupSelect : CaseData -> GroupListResponse -> Html Msg
displayGroupSelect caseInput groups =
    select [ onInput <| caseGroupChanged caseInput ] <|
        option [ value "", selected False ] [ text "<Private>" ]
            --option [ value "", selected (caseInput.groupId == Nothing) ] [ text "<Private>" ]
            :: List.map (determineSelected caseInput.groupId |> displayOption) groups


displayNewForm : CaseData -> Html Msg
displayNewForm caseInput =
    div [] <|
        fieldset []
            [ div [] [ input [ placeholder "Case reference", value caseInput.reference.value, onInput (caseReferenceChanged caseInput) ] [], displayValidity caseInput.reference.validity ]
            , div [] [ input [ type_ "datetime-local", placeholder "Deadline", value (Iso8601.fromTime caseInput.deadline.value |> String.dropRight 5), onInput (caseDeadlineChanged caseInput) ] [], displayValidity caseInput.deadline.validity ]
            , viewData (displayGroupSelect caseInput) caseInput.userGroups
            ]
            :: List.indexedMap (displayPrediction caseInput) caseInput.predictions
            ++ [ fieldset [] [ button [ onClick (addNewLine caseInput) ] [ text "+" ] ]
               , displayDebug caseInput
               ]



-- Parse URL


type Route
    = Users
    | User Id
    | Groups
    | Group Id
    | Case Id
    | New Id


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Users <| s "users"
        , map (User << Id) <| s "user" </> string
        , map (New << Id) <| s "user" </> string </> s "new"
        , map Groups <| s "groups"
        , map (Group << Id) <| s "group" </> string
        , map (Case << Id) <| s "case" </> string
        ]


parseUrlAndRequest : Model -> Url.Url -> ( Model, Cmd Msg )
parseUrlAndRequest model url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                Users ->
                    ( { model | state = UserList RemoteData.Loading }
                    , userListQuery |> makeRequest (UserList >> GotResponse)
                    )

                User id ->
                    ( { model | state = UserDetail RemoteData.Loading }
                    , id |> userDetailQuery |> makeRequest (UserDetail >> GotResponse)
                    )

                Groups ->
                    ( { model | state = GroupList RemoteData.Loading }
                    , groupListQuery identity |> makeRequest (GroupList >> GotResponse)
                    )

                Group id ->
                    ( { model | state = GroupDetail RemoteData.Loading }
                    , id |> groupDetailQuery |> makeRequest (GroupDetail >> GotResponse)
                    )

                Case id ->
                    ( { model | state = GroupDetail RemoteData.Loading }
                    , id |> caseDetailQuery |> makeRequest (CaseDetail >> GotResponse)
                    )

                New userId ->
                    let
                        withUserId args =
                            { args | userId = Present userId }

                        createCaseData creatorId groupList =
                            CaseData
                                creatorId
                                groupList
                                Nothing
                                (validateReference "")
                                [ blankPrediction ]
                                (validateDeadline "2021-08-18T12:00:00")
                    in
                    ( { model | state = NoData }
                    , groupListQuery withUserId |> makeRequest (createCaseData userId >> CaseChanged)
                    )

        Nothing ->
            ( { model | state = NoData }
            , Cmd.none
            )



-- New


preparePredictionInput : PredictionInput -> Predictions.InputObject.PredictionInput
preparePredictionInput predictionInput =
    Predictions.InputObject.PredictionInput predictionInput.diagnosis.value predictionInput.confidence.value


prepareNewCase : CaseData -> Predictions.InputObject.CaseInput
prepareNewCase caseInput =
    Predictions.InputObject.CaseInput
        caseInput.reference.value
        caseInput.creatorId
        OptionalArgument.Absent
        caseInput.deadline.value
        (List.map preparePredictionInput caseInput.predictions)


submitCase : CaseData -> Cmd Msg
submitCase caseInput =
    prepareNewCase caseInput
        |> Mutation.AddCaseRequiredArguments
        |> Mutation.addCase
        |> Graphql.Http.mutationRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> CaseCreated)



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


groupListQuery : (GroupsOptionalArguments -> GroupsOptionalArguments) -> SelectionSet GroupListResponse RootQuery
groupListQuery fillInOptionals =
    Query.groups fillInOptionals <| SelectionSet.map2 NamedNodeData Group.id Group.name


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
        |> Graphql.Http.queryRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> msgConstructor)


graphQlEndpoint =
    "http://localhost:3000/graphql"
