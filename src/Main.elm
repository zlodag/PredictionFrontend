module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import FormField exposing (Field)
import Graphql.Http exposing (Error, HttpError(..), Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, NamedNodeData, PredictionData, UserCandidate, blankPrediction, displayGroupSelect, validateConfidence, validateDeadline, viewData)
import Html exposing (Html, a, b, blockquote, button, dd, div, dl, dt, form, h4, hr, input, label, li, option, select, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (disabled, href, placeholder, selected, step, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import NewCase
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject exposing (CaseInput, PredictionInput)
import Predictions.Interface exposing (Event)
import Predictions.Interface.Event as Event
import Predictions.Mutation as Mutation
import Predictions.Object.Case as Case
import Predictions.Object.Comment as Comment
import Predictions.Object.CommentActivity
import Predictions.Object.DeadlineEvent
import Predictions.Object.Diagnosis as Diagnosis
import Predictions.Object.Group as Group
import Predictions.Object.GroupCaseActivity
import Predictions.Object.Judgement as Judgement
import Predictions.Object.JudgementActivity
import Predictions.Object.Score as Score
import Predictions.Object.User as User
import Predictions.Object.Wager as Wager
import Predictions.Object.WagerActivity
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import Round
import ScalarCodecs exposing (Id, Timestamp)
import String
import Svg as S
import Task
import Time
import Time.Distance
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)



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


type State
    = WelcomePage
    | UserDetail (GraphqlRemoteData UserDetailData)
    | ScoreDetail (List (CI.One Score CI.Dot)) (GraphqlRemoteData (List Score))
    | EventList (GraphqlRemoteData (List EventResult))
    | GroupList (GraphqlRemoteData (List NamedNodeData))
    | GroupDetail (GraphqlRemoteData GroupDetailData)
    | CaseDetail (GraphqlRemoteData CaseDetailData)
    | NewCase NewCase.Data
    | NoData


type alias Model =
    { key : Nav.Key
    , zone : Time.Zone
    , now : Time.Posix
    , state : State
    , auth : Auth
    , allUsers : List UserCandidate
    }


type Auth
    = SignedIn (List NamedNodeData) UserCandidate
    | SignedOut (List NamedNodeData)


newUserResponseHandler : GraphqlRemoteData Auth -> Msg
newUserResponseHandler response =
    UserChanged <|
        case response of
            Success auth ->
                auth

            _ ->
                SignedOut []


usersResponseHandler : Url.Url -> GraphqlRemoteData (List NamedNodeData) -> Msg
usersResponseHandler url response =
    GotAuth url <|
        SignedOut
            (case response of
                Success users ->
                    users

                _ ->
                    []
            )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key Time.utc (Time.millisToPosix 0) NoData (SignedOut []) []
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , userListQuery |> makeRequest (usersResponseHandler url)
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotAuth Url.Url Auth
    | GrabNewUser Id
    | UserChanged Auth
    | GotResponse State
    | UpdateCase NewCase.Data
    | SubmitCase CaseInput
    | SubmitComment CaseDetailData Id String
    | SubmitDeadline CaseDetailData Timestamp
    | SubmitGroup CaseDetailData (Maybe Id)
    | SubmitDiagnosis CaseDetailData Id PredictionInput
    | SubmitWager CaseDetailData Id Id Int
    | SubmitJudgement CaseDetailData Id Id Outcome
    | CaseCreated (GraphqlRemoteData Id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | now = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Task.perform Tick Time.now
            )

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

        GotAuth url auth ->
            parseUrlAndRequest { model | auth = auth } url

        GrabNewUser id ->
            ( model
            , SelectionSet.map2 SignedIn
                (Query.users <| SelectionSet.map2 NamedNodeData User.id User.name)
                (Query.user { id = id } <|
                    SelectionSet.map2 UserCandidate
                        (SelectionSet.map2 NamedNodeData User.id User.name)
                        (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)
                )
                |> makeRequest newUserResponseHandler
            )

        UserChanged auth ->
            --( { model | auth = auth, state = NoData }
            --, Nav.pushUrl model.key "/"
            --)
            ( { model | auth = auth }, Cmd.none )

        GotResponse graphqlRemoteData ->
            ( { model | state = graphqlRemoteData }
            , Cmd.none
            )

        UpdateCase caseData ->
            ( { model | state = NewCase caseData }
            , Cmd.none
            )

        CaseCreated graphqlRemoteData ->
            case graphqlRemoteData of
                Success (Id id) ->
                    ( model, Nav.pushUrl model.key <| "/case/" ++ id )

                _ ->
                    ( model, Cmd.none )

        SubmitCase caseData ->
            ( model, submitCase caseData )

        SubmitComment caseDetail creatorId string ->
            let
                withNewComment : CommentData -> CaseDetailData
                withNewComment newComment =
                    { caseDetail | comments = caseDetail.comments ++ [ newComment ] }
            in
            ( model
            , Mutation.addComment
                { creatorId = creatorId, caseId = caseDetail.node.id, text = string }
                mapToCommentData
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewComment caseDetail)
            )

        SubmitDeadline caseDetail deadline ->
            let
                withNewDeadline : Timestamp -> CaseDetailData
                withNewDeadline newDeadline =
                    { caseDetail | deadline = newDeadline }
            in
            ( model
            , Mutation.changeDeadline
                { caseId = caseDetail.node.id, newDeadline = deadline }
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewDeadline caseDetail)
            )

        SubmitGroup caseDetail groupId ->
            let
                fillInOptionals args =
                    { args
                        | newGroupId =
                            case groupId of
                                Just id ->
                                    Present id

                                Nothing ->
                                    Null
                    }

                withNewGroup : Maybe NamedNodeData -> CaseDetailData
                withNewGroup newGroup =
                    { caseDetail | group = newGroup }
            in
            ( model
            , Mutation.changeGroup fillInOptionals
                { caseId = caseDetail.node.id }
                (SelectionSet.map2 NamedNodeData Group.id Group.name)
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewGroup caseDetail)
            )

        SubmitDiagnosis caseDetail creatorId predictionInput ->
            let
                withNewDiagnosis : DiagnosisDetailData -> CaseDetailData
                withNewDiagnosis newDiagnosis =
                    { caseDetail | diagnoses = caseDetail.diagnoses ++ [ newDiagnosis ] }
            in
            ( model
            , Mutation.addDiagnosis
                { creatorId = creatorId, caseId = caseDetail.node.id, prediction = predictionInput }
                mapToDiagnosisDetailData
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewDiagnosis caseDetail)
            )

        SubmitWager caseDetail creatorId diagnosisId confidence ->
            let
                appendWager : WagerData -> DiagnosisDetailData -> DiagnosisDetailData
                appendWager newWager diagnosis =
                    if diagnosis.node.id == diagnosisId then
                        { diagnosis | wagers = diagnosis.wagers ++ [ newWager ] }

                    else
                        diagnosis

                withNewWager : WagerData -> CaseDetailData
                withNewWager newWager =
                    { caseDetail | diagnoses = List.map (appendWager newWager) caseDetail.diagnoses }
            in
            ( model
            , Mutation.addWager
                { creatorId = creatorId, diagnosisId = diagnosisId, confidence = confidence }
                mapToWagerData
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewWager caseDetail)
            )

        SubmitJudgement caseDetail creatorId diagnosisId outcome ->
            let
                addJudgement : JudgementData -> DiagnosisDetailData -> DiagnosisDetailData
                addJudgement newJudgement diagnosis =
                    if diagnosis.node.id == diagnosisId then
                        { diagnosis | judgement = Just newJudgement }

                    else
                        diagnosis

                withNewJudgement : JudgementData -> CaseDetailData
                withNewJudgement newJudgement =
                    { caseDetail | diagnoses = List.map (addJudgement newJudgement) caseDetail.diagnoses }
            in
            ( model
            , Mutation.judgeOutcome
                { diagnosisId = diagnosisId, judgedById = creatorId, outcome = outcome }
                mapToJudgementData
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> updateCase withNewJudgement caseDetail)
            )


updateCase : (a -> CaseDetailData) -> CaseDetailData -> GraphqlRemoteData a -> Msg
updateCase getUpdatedCase oldCase newData =
    (case newData of
        Success data ->
            getUpdatedCase data |> (\c -> { c | state = Viewing })

        _ ->
            oldCase
    )
        |> Success
        |> CaseDetail
        |> GotResponse



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- update every minute
    Time.every 60000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Predictions"
    , body =
        [ displayAuth model.auth
        , ul [] <|
            [ li [] [ a [ href "/" ] [ text "Home" ] ]
            , li [] [ a [ href "/graphql" ] [ text "GraphiQL" ] ]
            ]
                ++ (case model.auth of
                        SignedIn _ userCandidate ->
                            case userCandidate.node.id of
                                Id userId ->
                                    [ li [] [ a [ href <| "/user/" ++ userId ] [ text "My user page" ] ]
                                    , li [] [ a [ href <| "/user/" ++ userId ++ "/events" ] [ text "Events" ] ]
                                    , li [] [ a [ href "/new" ] [ text "New prediction" ] ]
                                    ]

                        SignedOut _ ->
                            []
                   )
        , hr [] []
        , displayData model
        ]
    }


determineNewAuth : Auth -> String -> Msg
determineNewAuth auth id =
    let
        users =
            case auth of
                SignedIn a _ ->
                    a

                SignedOut a ->
                    a
    in
    case List.filter (\user -> user.id == Id id) users of
        [ newUser ] ->
            GrabNewUser newUser.id

        _ ->
            UserChanged <| SignedOut users


displayAuth : Auth -> Html Msg
displayAuth auth =
    let
        ( allUsers, isLoggedInUser, loggedOut ) =
            case auth of
                SignedIn users currentUser ->
                    ( users, \userId -> currentUser.node.id == userId, False )

                SignedOut users ->
                    ( users, always False, True )

        mapToOption : NamedNodeData -> Html msg
        mapToOption user =
            case user.id of
                Id id ->
                    option [ value id, selected <| isLoggedInUser user.id ] [ text user.name ]
    in
    div []
        [ label [] [ text "Select current user: " ]
        , select [ onInput <| determineNewAuth auth ] <|
            option [ value "", selected loggedOut ] [ text "<No user>" ]
                :: List.map mapToOption allUsers
        ]


displayData : Model -> Html Msg
displayData model =
    case model.state of
        WelcomePage ->
            text <| welcomeMessage model.auth

        NoData ->
            text "No data!"

        UserDetail graphqlRemoteData ->
            viewData (displayUser model.now) graphqlRemoteData

        ScoreDetail hovering graphqlRemoteData ->
            viewData (displayScores model.zone model.now hovering) graphqlRemoteData

        EventList graphqlRemoteData ->
            viewData (displayEvents model.now) graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group") graphqlRemoteData

        GroupDetail graphqlRemoteData ->
            viewData displayGroup graphqlRemoteData

        CaseDetail graphqlRemoteData ->
            viewData (displayCase model.now model.auth) graphqlRemoteData

        NewCase caseData ->
            case model.auth of
                SignedIn _ currentUser ->
                    NewCase.displayNewForm UpdateCase SubmitCase currentUser caseData

                _ ->
                    text "Sign in first"


welcomeMessage : Auth -> String
welcomeMessage auth =
    case auth of
        SignedIn _ userCandidate ->
            "Welcome, " ++ userCandidate.node.name

        SignedOut _ ->
            "Please sign in to begin"


displayNamedNodeLink : String -> NamedNodeData -> Html msg
displayNamedNodeLink base_path node =
    case node.id of
        Id id ->
            a [ href <| base_path ++ "/" ++ id ] [ text node.name ]


displayNamedNodeList : String -> List NamedNodeData -> Html msg
displayNamedNodeList base_path nodes =
    ul [] <| displayListItems (displayNamedNodeLink base_path >> List.singleton) nodes


displayListItems : (a -> List (Html msg)) -> List a -> List (Html msg)
displayListItems displayFunction list =
    List.map (li [] << displayFunction) list


displayUser : Time.Posix -> UserDetailData -> Html msg
displayUser now user =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/user" user.node ]
        , dt [] [ text "Created" ]
        , dd [] [ displayTime user.created now ]
        , dt [] [ text "Score" ]
        , dd []
            [ case user.score of
                Just score ->
                    a
                        [ href <|
                            "/user/"
                                ++ (case user.node.id of
                                        Id id ->
                                            id
                                   )
                                ++ "/score"
                        ]
                        [ text <| Round.round 4 score ]

                Nothing ->
                    text "No predictions judged yet"
            ]
        , dt [] [ text <| "Groups (" ++ String.fromInt (List.length user.groups) ++ ")" ]
        , dd [] [ displayNamedNodeList "/group" user.groups ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length user.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case" user.cases ]
        ]


displayScore : Time.Posix -> Score -> List (Html msg)
displayScore now score =
    [ td [] [ displayTime score.judged now ]
    , td [] [ displayNamedNodeLink "/case" score.case_ ]
    , td [] [ text score.diagnosis ]
    , td [] [ text <| String.fromInt score.confidence ++ "%" ]
    , td []
        [ text
            (case score.outcome of
                Right ->
                    "✔"

                Wrong ->
                    "✗"

                Indeterminate ->
                    "?"
            )
        ]
    , td [] [ text <| Round.round 4 score.brierScore ]
    , td [] [ text <| Round.round 4 score.averageBrierScore ]
    , td [] [ text <| Round.round 4 score.adjustedBrierScore ]
    ]


displayChart : Time.Zone -> (List (CI.One Score CI.Dot) -> Msg) -> List (CI.One Score CI.Dot) -> List Score -> Html Msg
displayChart zone onHover hovering scores =
    let
        formatTime : Time.Posix -> String
        formatTime time =
            (String.fromInt <| Time.toDay zone time)
                ++ "/"
                ++ (case Time.toMonth zone time of
                        Time.Jan ->
                            "01"

                        Time.Feb ->
                            "02"

                        Time.Mar ->
                            "03"

                        Time.Apr ->
                            "04"

                        Time.May ->
                            "05"

                        Time.Jun ->
                            "06"

                        Time.Jul ->
                            "07"

                        Time.Aug ->
                            "08"

                        Time.Sep ->
                            "09"

                        Time.Oct ->
                            "10"

                        Time.Nov ->
                            "11"

                        Time.Dec ->
                            "12"
                   )
                ++ "/"
                ++ (String.dropLeft 2 <| String.fromInt <| Time.toYear zone time)
    in
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
            , CA.format (floor >> Time.millisToPosix >> formatTime)
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


displayScores : Time.Zone -> Time.Posix -> List (CI.One Score CI.Dot) -> List Score -> Html Msg
displayScores zone now hovering scores =
    let
        onHover : List (CI.One Score CI.Dot) -> Msg
        onHover newHovering =
            GotResponse <| ScoreDetail newHovering <| Success scores
    in
    div []
        [ div [ Html.Attributes.style "width" "500px" ] [ displayChart zone onHover hovering scores ]
        , div [ Html.Attributes.style "width" "500px" ] [ displayConfidenceHistogram scores ]
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
            , tbody [] <| List.map (displayScore now >> tr []) scores
            ]
        ]


displayTime : Time.Posix -> Time.Posix -> Html msg
displayTime timeToDisplay now =
    span [ title <| Iso8601.fromTime timeToDisplay ] [ text <| " " ++ Time.Distance.inWords timeToDisplay now ]


displayEvent : Time.Posix -> EventResult -> Html msg
displayEvent now event =
    li []
        (case event of
            WagerActivity activityData diagnosis confidence ->
                [ displayTime activityData.event.timestamp now
                , text ": "
                , displayNamedNodeLink "/user" activityData.user
                , text <| " added a wager on "
                , displayNamedNodeLink "/case" activityData.event.case_
                , text <| ": " ++ diagnosis ++ " (" ++ String.fromInt confidence ++ "%)"
                ]

            JudgementActivity activityData diagnosis outcome ->
                [ displayTime activityData.event.timestamp now
                , text ": "
                , displayNamedNodeLink "/user" activityData.user
                , text <| " judged '" ++ diagnosis ++ "' as " ++ displayOutcome outcome ++ " on case: "
                , displayNamedNodeLink "/case" activityData.event.case_
                ]

            CommentActivity activityData comment ->
                [ displayTime activityData.event.timestamp now
                , text ": "
                , displayNamedNodeLink "/user" activityData.user
                , text " commented on "
                , displayNamedNodeLink "/case" activityData.event.case_
                , text <| ": '" ++ comment ++ "'"
                ]

            GroupCaseActivity activityData group ->
                [ displayTime activityData.event.timestamp now
                , text ": "
                , displayNamedNodeLink "/user" activityData.user
                , text " added a case in group "
                , displayNamedNodeLink "/group" group
                , text ": "
                , displayNamedNodeLink "/case" activityData.event.case_
                ]

            DeadlineEvent eventData ->
                [ displayTime eventData.timestamp now
                , text ": Deadline reached for "
                , displayNamedNodeLink "/case" eventData.case_
                , text "!"
                ]
        )


displayEvents : Time.Posix -> List EventResult -> Html msg
displayEvents now events =
    ul [] <| List.map (displayEvent now) events


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


cancelEditButton : CaseDetailData -> Html Msg
cancelEditButton caseDetail =
    button [ type_ "button", onClick <| changeCase { caseDetail | state = Viewing } ] [ text "Cancel" ]


displayCaseGroup : Auth -> CaseDetailData -> List (Html Msg)
displayCaseGroup auth caseDetail =
    let
        ( viewGroup, groupId ) =
            case caseDetail.group of
                Just g ->
                    ( [ displayNamedNodeLink "/group" g ], Just g.id )

                Nothing ->
                    ( [ text "None" ], Nothing )
    in
    case auth of
        SignedIn _ user ->
            if user.node.id == caseDetail.creator.id then
                case caseDetail.state of
                    ChangingGroup groups ->
                        [ div [] [ displayGroupSelect groupId groups <| SubmitGroup caseDetail ]
                        , div [] [ cancelEditButton caseDetail ]
                        ]

                    Viewing ->
                        [ div [] viewGroup
                        , div [] [ button [ onClick <| changeCase { caseDetail | state = ChangingGroup user.groups } ] [ text "Change" ] ]
                        ]

                    _ ->
                        viewGroup

            else
                viewGroup

        _ ->
            viewGroup


displayDeadline : Time.Posix -> Auth -> CaseDetailData -> List (Html Msg)
displayDeadline now auth caseDetail =
    let
        viewDeadline =
            [ displayTime caseDetail.deadline now ]
    in
    case auth of
        SignedIn _ user ->
            if user.node.id == caseDetail.creator.id then
                case caseDetail.state of
                    ChangingDeadline newDeadline ->
                        let
                            changeDeadline : Field Timestamp -> Msg
                            changeDeadline deadline =
                                changeCase { caseDetail | state = ChangingDeadline deadline }

                            ( submitButtonDisabled, formAttributes ) =
                                case FormField.getValue newDeadline of
                                    Ok deadline ->
                                        ( False, [ onSubmit <| SubmitDeadline caseDetail deadline ] )

                                    Err _ ->
                                        ( True, [] )
                        in
                        [ form formAttributes
                            [ input [ type_ "datetime-local", FormField.withValue newDeadline, FormField.onInput changeDeadline newDeadline ] []
                            , div []
                                [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                , cancelEditButton caseDetail
                                ]
                            ]
                        ]

                    Viewing ->
                        [ div [] viewDeadline
                        , div [] [ button [ onClick <| changeCase { caseDetail | state = ChangingDeadline <| FormField.newField validateDeadline } ] [ text "Change" ] ]
                        ]

                    _ ->
                        viewDeadline

            else
                viewDeadline

        SignedOut _ ->
            viewDeadline


displayCase : Time.Posix -> Auth -> CaseDetailData -> Html Msg
displayCase now auth caseDetail =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNodeLink "/case" caseDetail.node ]
        , dt [] [ text "Deadline" ]
        , dd [] <| displayDeadline now auth caseDetail
        , dt [] [ text "Creator" ]
        , dd [] [ displayNamedNodeLink "/user" caseDetail.creator ]
        , dt [] [ text "Group" ]
        , dd [] <| displayCaseGroup auth caseDetail
        , dt [] [ text <| "Diagnoses (" ++ String.fromInt (List.length caseDetail.diagnoses) ++ ")" ]
        , dd [] [ ul [] <| displayListItems (displayDiagnosis now auth caseDetail) caseDetail.diagnoses ++ displayNewDiagnosis auth caseDetail ]
        , dt [] [ text <| "Comments (" ++ String.fromInt (List.length caseDetail.comments) ++ ")" ]
        , dd []
            [ ul [] <|
                displayListItems (displayComment now) caseDetail.comments
                    ++ displayNewComment auth caseDetail
            ]
        ]


displayDiagnosis : Time.Posix -> Auth -> CaseDetailData -> DiagnosisDetailData -> List (Html Msg)
displayDiagnosis now auth caseDetail diagnosis =
    let
        judgedAsBy outcome judgedBy =
            [ text " Judged as " ] ++ outcome ++ [ text " by ", displayNamedNodeLink "/user" judgedBy ]
    in
    [ h4 [] [ text diagnosis.node.name ]
    , ul [] <|
        displayListItems (displayWager now) diagnosis.wagers
            ++ (case diagnosis.judgement of
                    Just judgementData ->
                        judgedAsBy [ strong [] [ text <| displayOutcome judgementData.outcome ] ] judgementData.judgedBy
                            ++ [ displayTime judgementData.timestamp now ]
                            |> li []
                            |> List.singleton

                    Nothing ->
                        let
                            changeWager : Field Int -> Msg
                            changeWager field =
                                changeCase { caseDetail | state = AddingWager diagnosis.node.id field }
                        in
                        case ( auth, caseDetail.state ) of
                            ( SignedIn _ user, Viewing ) ->
                                (if List.any (\wager -> wager.creator.id == user.node.id) diagnosis.wagers then
                                    []

                                 else
                                    [ button
                                        [ type_ "button", onClick <| changeWager <| FormField.newField validateConfidence ]
                                        [ text "Add wager" ]
                                    ]
                                )
                                    ++ [ button
                                            [ type_ "button", onClick <| changeCase { caseDetail | state = Judging diagnosis.node.id } ]
                                            [ text "Judge" ]
                                       ]
                                    |> li []
                                    |> List.singleton

                            ( SignedIn _ user, Judging id ) ->
                                let
                                    judgeOutcomeButton outcome label =
                                        button [ type_ "button", onClick <| SubmitJudgement caseDetail user.node.id diagnosis.node.id outcome ] [ text label ]
                                in
                                if id == diagnosis.node.id then
                                    judgedAsBy [ judgeOutcomeButton Right "Right", judgeOutcomeButton Wrong "Wrong", judgeOutcomeButton Indeterminate "Indeterminate" ]
                                        user.node
                                        ++ [ div [] [ cancelEditButton caseDetail ] ]
                                        |> li []
                                        |> List.singleton

                                else
                                    []

                            ( SignedIn _ user, AddingWager id newWager ) ->
                                let
                                    ( submitButtonDisabled, formAttributes ) =
                                        case FormField.getValue newWager of
                                            Ok confidence ->
                                                ( False, [ onSubmit <| SubmitWager caseDetail user.node.id diagnosis.node.id confidence ] )

                                            Err _ ->
                                                ( True, [] )
                                in
                                if id == diagnosis.node.id then
                                    [ form formAttributes
                                        [ displayNamedNodeLink "/user" user.node
                                        , text " estimated "
                                        , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue newWager, FormField.onInput changeWager newWager ] []
                                        , text "%"
                                        , div []
                                            [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                            , cancelEditButton caseDetail
                                            ]
                                        ]
                                    ]
                                        |> li []
                                        |> List.singleton

                                else
                                    []

                            _ ->
                                []
               )
    ]


displayNewDiagnosis : Auth -> CaseDetailData -> List (Html Msg)
displayNewDiagnosis auth caseDetail =
    case ( auth, caseDetail.state ) of
        ( SignedIn _ _, Viewing ) ->
            [ button
                [ type_ "button", onClick <| changeCase { caseDetail | state = AddingDiagnosis blankPrediction } ]
                [ text "Add diagnosis" ]
            ]
                |> h4 []
                |> List.singleton
                |> li []
                |> List.singleton

        ( SignedIn _ user, AddingDiagnosis prediction ) ->
            let
                changePrediction newPrediction =
                    changeCase { caseDetail | state = AddingDiagnosis newPrediction }

                changeDiagnosis diagnosis =
                    changePrediction { prediction | diagnosis = diagnosis }

                changeConfidence confidence =
                    changePrediction { prediction | confidence = confidence }

                ( submitButtonDisabled, formAttributes ) =
                    case ( FormField.getValue prediction.diagnosis, FormField.getValue prediction.confidence ) of
                        ( Ok diagnosis, Ok confidence ) ->
                            ( False, [ onSubmit <| SubmitDiagnosis caseDetail user.node.id <| PredictionInput diagnosis confidence ] )

                        _ ->
                            ( True, [] )
            in
            [ form formAttributes
                [ h4 [] [ input [ type_ "text", placeholder "Diagnosis", FormField.withValue prediction.diagnosis, FormField.onInput changeDiagnosis prediction.diagnosis ] [] ]
                , ul []
                    [ li []
                        [ displayNamedNodeLink "/user" user.node
                        , text " estimated "
                        , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue prediction.confidence, FormField.onInput changeConfidence prediction.confidence ] []
                        , text "%"
                        ]
                    ]
                , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                , cancelEditButton caseDetail
                ]
            ]
                |> li []
                |> List.singleton

        _ ->
            []


changeCase : CaseDetailData -> Msg
changeCase newCase =
    newCase |> Success >> CaseDetail >> GotResponse


displayNewComment : Auth -> CaseDetailData -> List (Html Msg)
displayNewComment auth caseDetail =
    let
        changeComment : Field String -> Msg
        changeComment comment =
            changeCase { caseDetail | state = AddingComment comment }
    in
    case ( auth, caseDetail.state ) of
        ( SignedIn _ user, AddingComment newComment ) ->
            let
                ( submitButtonDisabled, formAttributes ) =
                    case FormField.getValue newComment of
                        Ok comment ->
                            ( False, [ onSubmit <| SubmitComment caseDetail user.node.id comment ] )

                        Err _ ->
                            ( True, [] )
            in
            [ li []
                [ displayNamedNodeLink "/user" user.node
                , blockquote []
                    [ form formAttributes
                        [ input [ type_ "text", placeholder "Enter comment", FormField.withValue newComment, FormField.onInput changeComment newComment ] []
                        , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                        , cancelEditButton caseDetail
                        ]
                    ]
                ]
            ]

        ( SignedIn _ _, Viewing ) ->
            [ li [] [ button [ onClick <| changeComment <| FormField.newNonEmptyStringField "Comment" ] [ text "Add comment" ] ] ]

        _ ->
            []


displayComment : Time.Posix -> CommentData -> List (Html msg)
displayComment now comment =
    [ displayNamedNodeLink "/user" comment.creator
    , displayTime comment.timestamp now
    , blockquote [] [ text comment.text ]
    ]


displayWager : Time.Posix -> WagerData -> List (Html msg)
displayWager now wager =
    [ displayNamedNodeLink "/user" wager.creator
    , text " estimated "
    , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
    , displayTime wager.timestamp now
    ]


displayOutcome : Outcome -> String
displayOutcome outcome =
    case outcome of
        Right ->
            "Right"

        Wrong ->
            "Wrong"

        Indeterminate ->
            "Indeterminate"



-- Parse URL


type Route
    = Welcome
    | User Id
    | UserScore Id
    | Events Id
    | Groups
    | Group Id
    | Case Id
    | New


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Welcome top
        , map (User << Id) <| s "user" </> string
        , map (UserScore << Id) <| s "user" </> string </> s "score"
        , map (Events << Id) <| s "user" </> string </> s "events"
        , map Groups <| s "groups"
        , map (Group << Id) <| s "group" </> string
        , map (Case << Id) <| s "case" </> string
        , map New <| s "new"
        ]


parseUrlAndRequest : Model -> Url.Url -> ( Model, Cmd Msg )
parseUrlAndRequest model url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                Welcome ->
                    ( { model | state = WelcomePage }, Cmd.none )

                User id ->
                    ( model
                    , Query.user { id = id } mapToUserDetailData |> makeRequest (UserDetail >> GotResponse)
                    )

                UserScore userId ->
                    ( model
                    , Query.user { id = userId } (User.scores <| mapToScore) |> makeRequest (ScoreDetail [] >> GotResponse)
                    )

                Events userId ->
                    let
                        mapToEventData mapToCaseId mapToCaseReference mapToTimestamp =
                            SelectionSet.map2 EventData
                                (SelectionSet.map2 NamedNodeData mapToCaseId mapToCaseReference)
                                mapToTimestamp

                        mapToActivityData mapToCaseId mapToCaseReference mapToTimestamp mapToUserId mapToUserName =
                            SelectionSet.map2 ActivityData
                                (mapToEventData mapToCaseId mapToCaseReference mapToTimestamp)
                                (SelectionSet.map2 NamedNodeData mapToUserId mapToUserName)

                        mapToWagerActivity =
                            SelectionSet.map3 WagerActivity
                                (mapToActivityData Predictions.Object.WagerActivity.caseId Predictions.Object.WagerActivity.caseReference Predictions.Object.WagerActivity.timestamp Predictions.Object.WagerActivity.userId Predictions.Object.WagerActivity.userName)
                                Predictions.Object.WagerActivity.diagnosis
                                Predictions.Object.WagerActivity.confidence

                        mapToJudgementActivity =
                            SelectionSet.map3 JudgementActivity
                                (mapToActivityData Predictions.Object.JudgementActivity.caseId Predictions.Object.JudgementActivity.caseReference Predictions.Object.JudgementActivity.timestamp Predictions.Object.JudgementActivity.userId Predictions.Object.JudgementActivity.userName)
                                Predictions.Object.JudgementActivity.diagnosis
                                Predictions.Object.JudgementActivity.outcome

                        mapToCommentActivity =
                            SelectionSet.map2 CommentActivity
                                (mapToActivityData Predictions.Object.CommentActivity.caseId Predictions.Object.CommentActivity.caseReference Predictions.Object.CommentActivity.timestamp Predictions.Object.CommentActivity.userId Predictions.Object.CommentActivity.userName)
                                Predictions.Object.CommentActivity.comment

                        mapToGroupCaseActivity =
                            SelectionSet.map2 GroupCaseActivity
                                (mapToActivityData Predictions.Object.GroupCaseActivity.caseId Predictions.Object.GroupCaseActivity.caseReference Predictions.Object.GroupCaseActivity.timestamp Predictions.Object.GroupCaseActivity.userId Predictions.Object.GroupCaseActivity.userName)
                                (SelectionSet.map2 NamedNodeData Predictions.Object.GroupCaseActivity.groupId Predictions.Object.GroupCaseActivity.groupName)

                        mapToDeadlineEvent =
                            SelectionSet.map DeadlineEvent
                                (mapToEventData Predictions.Object.DeadlineEvent.caseId Predictions.Object.DeadlineEvent.caseReference Predictions.Object.DeadlineEvent.timestamp)

                        mapToEventResult : SelectionSet EventResult Event
                        mapToEventResult =
                            Event.fragments
                                { onWagerActivity = mapToWagerActivity
                                , onJudgementActivity = mapToJudgementActivity
                                , onCommentActivity = mapToCommentActivity
                                , onGroupCaseActivity = mapToGroupCaseActivity
                                , onDeadlineEvent = mapToDeadlineEvent
                                }
                    in
                    ( model
                    , Query.events (\args -> { args | limit = Present 100 }) { userId = userId } mapToEventResult |> makeRequest (EventList >> GotResponse)
                    )

                Groups ->
                    ( model
                    , Query.groups identity (SelectionSet.map2 NamedNodeData Group.id Group.name) |> makeRequest (GroupList >> GotResponse)
                    )

                Group id ->
                    ( model
                    , Query.group { id = id } mapToGroupDetailData |> makeRequest (GroupDetail >> GotResponse)
                    )

                Case id ->
                    ( model
                    , Query.case_ { id = id } mapToCaseDetailData |> makeRequest (CaseDetail >> GotResponse)
                    )

                New ->
                    case model.auth of
                        SignedIn _ _ ->
                            ( { model
                                | state =
                                    NewCase NewCase.blankCase
                              }
                            , Cmd.none
                            )

                        SignedOut _ ->
                            ( { model | state = NoData }
                            , Cmd.none
                            )

        Nothing ->
            ( { model | state = NoData }
            , Cmd.none
            )



-- New


submitCase : CaseInput -> Cmd Msg
submitCase caseData =
    Mutation.addCase { caseInput = caseData }
        |> Graphql.Http.mutationRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> CaseCreated)


type alias UserDetailData =
    { node : NamedNodeData
    , created : Timestamp
    , score : Maybe Float
    , groups : List NamedNodeData
    , cases : List CaseLimitedData
    }


mapToUserDetailData =
    SelectionSet.map5 UserDetailData
        (SelectionSet.map2 NamedNodeData User.id User.name)
        User.created
        (User.score { adjusted = False })
        (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        (User.cases <| SelectionSet.map2 NamedNodeData Case.id Case.reference)


type alias UserDetailResponse =
    Maybe UserDetailData


userListQuery : SelectionSet (List NamedNodeData) RootQuery
userListQuery =
    Query.users <| SelectionSet.map2 NamedNodeData User.id User.name


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


mapToScore =
    SelectionSet.map8 Score
        Score.judged
        (SelectionSet.map2 NamedNodeData Score.caseId Score.reference)
        Score.diagnosis
        Score.confidence
        Score.outcome
        Score.brierScore
        Score.averageBrierScore
        Score.adjustedBrierScore


type alias EventData =
    { case_ : NamedNodeData
    , timestamp : Timestamp
    }


type alias ActivityData =
    { event : EventData
    , user : NamedNodeData
    }


type EventResult
    = WagerActivity ActivityData String Int
    | JudgementActivity ActivityData String Outcome
    | CommentActivity ActivityData String
    | GroupCaseActivity ActivityData NamedNodeData
    | DeadlineEvent EventData


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


type alias CaseLimitedData =
    NamedNodeData


type alias CommentData =
    { creator : NamedNodeData
    , timestamp : Timestamp
    , text : String
    }


mapToCommentData =
    SelectionSet.map3 CommentData
        (Comment.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        Comment.timestamp
        Comment.text


type CaseDetailState
    = Viewing
    | ChangingDeadline (Field Timestamp)
    | ChangingGroup (List NamedNodeData)
    | AddingDiagnosis PredictionData
    | AddingWager Id (Field Int)
    | Judging Id
    | AddingComment (Field String)


type alias CaseDetailData =
    { state : CaseDetailState
    , node : NamedNodeData
    , creator : NamedNodeData
    , group : Maybe NamedNodeData
    , deadline : Timestamp
    , diagnoses : List DiagnosisDetailData
    , comments : List CommentData
    }


mapToCaseDetailData =
    SelectionSet.map6 (CaseDetailData Viewing)
        (SelectionSet.map2 NamedNodeData Case.id Case.reference)
        (Case.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        (Case.group <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        Case.deadline
        (Case.diagnoses <| mapToDiagnosisDetailData)
        (Case.comments <| mapToCommentData)


type alias CaseDetailResponse =
    Maybe CaseDetailData


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
