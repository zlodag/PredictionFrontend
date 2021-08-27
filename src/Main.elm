module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Graphql.Http exposing (Error, HttpError(..), Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, viewData)
import Html exposing (Html, a, b, blockquote, button, dd, div, dl, dt, fieldset, form, h4, hr, input, label, li, option, select, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (disabled, href, placeholder, required, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601 exposing (fromTime)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject exposing (CaseInput, PredictionInput)
import Predictions.Mutation as Mutation
import Predictions.Object.Case as Case
import Predictions.Object.Comment as Comment
import Predictions.Object.Diagnosis as Diagnosis
import Predictions.Object.Group as Group
import Predictions.Object.Judgement as Judgement
import Predictions.Object.Score as Score
import Predictions.Object.User as User
import Predictions.Object.Wager as Wager
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import Round
import ScalarCodecs exposing (Id, Timestamp)
import String
import Time
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


type alias PredictionData =
    { diagnosis : FormField String
    , confidence : FormField Int
    }


type alias InputValue =
    String


type alias ValidationMessage =
    String


type FormField a
    = Valid a InputValue
    | Invalid ValidationMessage InputValue


type alias CaseData =
    { groupId : Maybe Id
    , reference : FormField String
    , predictions : List PredictionData
    , deadline : FormField Timestamp
    }


type State
    = WelcomePage
    | UserDetail (GraphqlRemoteData UserDetailData)
    | ScoreDetail (GraphqlRemoteData (List Score))
    | GroupList (GraphqlRemoteData (List NamedNodeData))
    | GroupDetail (GraphqlRemoteData GroupDetailData)
    | CaseDetail (GraphqlRemoteData CaseDetailData)
    | NewCase CaseData
    | NoData


type alias Model =
    { key : Nav.Key
    , state : State
    , auth : Auth
    , allUsers : List UserCandidate
    }


type Auth
    = SignedIn (List UserCandidate) UserCandidate
    | SignedOut (List UserCandidate)


usersResponseHandler : Url.Url -> GraphqlRemoteData UserListResponse -> Msg
usersResponseHandler url response =
    GotAuth url <|
        case response of
            Success users ->
                case List.head users of
                    Just a ->
                        SignedIn users a

                    Nothing ->
                        SignedOut users

            _ ->
                SignedOut []


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key NoData (SignedOut []) []
    , userListQuery |> makeRequest (usersResponseHandler url)
    )



--parseUrlAndRequest (Model key NoData Nothing) url
-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotAuth Url.Url Auth
    | UserChanged Auth
    | GotResponse State
    | ChangeNewCase CaseData
    | SubmitCase Id CaseInput
    | SubmitComment CaseDetailData Id String
    | SubmitGroup CaseDetailData (Maybe Id)
    | SubmitDiagnosis CaseDetailData Id PredictionInput
    | SubmitWager CaseDetailData Id Id Int
    | SubmitJudgement CaseDetailData Id Id Outcome
    | CaseCreated (GraphqlRemoteData Id)


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

        GotAuth url auth ->
            parseUrlAndRequest { model | auth = auth } url

        UserChanged auth ->
            --( { model | auth = auth, state = NoData }
            --, Nav.pushUrl model.key "/"
            --)
            ( { model | auth = auth }, Cmd.none )

        GotResponse graphqlRemoteData ->
            ( { model | state = graphqlRemoteData }
            , Cmd.none
            )

        ChangeNewCase caseData ->
            ( { model | state = NewCase caseData }
            , Cmd.none
            )

        CaseCreated graphqlRemoteData ->
            case graphqlRemoteData of
                Success (Id id) ->
                    ( model, Nav.pushUrl model.key <| "/case/" ++ id )

                _ ->
                    ( model, Cmd.none )

        SubmitCase creatorId caseData ->
            ( model, submitCase { caseData | creatorId = creatorId } )

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
    Sub.none



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
                                    , li [] [ a [ href "/new" ] [ text "New prediction" ] ]
                                    ]

                        SignedOut _ ->
                            []
                   )
        , hr [] []
        , displayData model
        ]
    }


determineNewAuth : Auth -> String -> Auth
determineNewAuth auth id =
    let
        users =
            case auth of
                SignedIn a _ ->
                    a

                SignedOut a ->
                    a
    in
    case List.filter (\user -> user.node.id == Id id) users of
        [ newUser ] ->
            SignedIn users newUser

        _ ->
            SignedOut users


displayAuth : Auth -> Html Msg
displayAuth auth =
    div []
        [ label [] [ text "Select current user: " ]
        , select [ onInput <| determineNewAuth auth >> UserChanged ] <|
            List.map
                (determineSelected
                    (case auth of
                        SignedIn _ currentUser ->
                            Just currentUser.node.id

                        SignedOut _ ->
                            Nothing
                    )
                    |> displayOption
                )
            <|
                NamedNodeData (Id "") "<No user>"
                    :: List.map (\user -> user.node)
                        (case auth of
                            SignedIn users _ ->
                                users

                            SignedOut users ->
                                users
                        )
        ]


displayData : Model -> Html Msg
displayData model =
    case model.state of
        WelcomePage ->
            text <| welcomeMessage model.auth

        NoData ->
            text "No data!"

        UserDetail graphqlRemoteData ->
            viewData displayUser graphqlRemoteData

        ScoreDetail graphqlRemoteData ->
            viewData displayScores graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group") graphqlRemoteData

        GroupDetail graphqlRemoteData ->
            viewData displayGroup graphqlRemoteData

        CaseDetail graphqlRemoteData ->
            viewData (displayCase model.auth) graphqlRemoteData

        NewCase caseData ->
            case model.auth of
                SignedIn _ currentUser ->
                    displayNewForm currentUser caseData

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


displayUser : UserDetailData -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/user" user.node ]
        , dt [] [ text "Created" ]
        , dd [] [ text <| fromTime user.created ]
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


displayScore : Score -> List (Html msg)
displayScore score =
    --let
    --    ( diagnosisCellElement, outcomeCell, scoreCell ) =
    --        case score.outcome of
    --            Determined right brierScore ->
    --                ( text score.diagnosis
    --                , [ text
    --                        (if right then
    --                            "✔"
    --
    --                         else
    --                            "✗"
    --                        )
    --                  ]
    --                , [ text <| Round.round 4 brierScore ]
    --                )
    --
    --            IndeterminateScore ->
    --                ( Html.s [] [ text score.diagnosis ], [], [] )
    --in
    [ td [] [ text <| Iso8601.fromTime score.judged ]
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


displayScores : ScoresListData -> Html msg
displayScores scores =
    table []
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
        , tbody [] <| List.map (displayScore >> tr []) scores
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


cancelEditButton : CaseDetailData -> Html Msg
cancelEditButton caseDetail =
    button [ type_ "button", onClick <| changeCase { caseDetail | state = Viewing } ] [ text "Cancel" ]


displayCaseGroup : Auth -> CaseDetailData -> List (Html Msg)
displayCaseGroup auth caseDetail =
    let
        viewGroup =
            case caseDetail.group of
                Just g ->
                    displayNamedNodeLink "/group" g

                Nothing ->
                    text "None"

        groupId =
            case caseDetail.group of
                Just g ->
                    Just g.id

                Nothing ->
                    Nothing
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
                        [ div [] [ viewGroup ]
                        , div [] [ button [ onClick <| changeCase { caseDetail | state = ChangingGroup user.groups } ] [ text "Change" ] ]
                        ]

                    _ ->
                        [ viewGroup ]

            else
                [ viewGroup ]

        _ ->
            [ viewGroup ]


displayCase : Auth -> CaseDetailData -> Html Msg
displayCase auth caseDetail =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNodeLink "/case" caseDetail.node ]
        , dt [] [ text "Deadline" ]
        , dd [] [ text <| fromTime caseDetail.deadline ]
        , dt [] [ text "Creator" ]
        , dd [] [ displayNamedNodeLink "/user" caseDetail.creator ]
        , dt [] [ text "Group" ]
        , dd [] <| displayCaseGroup auth caseDetail
        , dt [] [ text <| "Diagnoses (" ++ String.fromInt (List.length caseDetail.diagnoses) ++ ")" ]
        , dd [] [ ul [] <| displayListItems (displayDiagnosis auth caseDetail) caseDetail.diagnoses ++ displayNewDiagnosis auth caseDetail ]
        , dt [] [ text <| "Comments (" ++ String.fromInt (List.length caseDetail.comments) ++ ")" ]
        , dd []
            [ ul [] <|
                displayListItems displayComment caseDetail.comments
                    ++ displayNewComment auth caseDetail
            ]
        ]


displayDiagnosis : Auth -> CaseDetailData -> DiagnosisDetailData -> List (Html Msg)
displayDiagnosis auth caseDetail diagnosis =
    let
        changeWager : String -> Msg
        changeWager newValue =
            changeCase { caseDetail | state = AddingWager diagnosis.node.id <| validateConfidence newValue }

        judgedAsBy outcome judgedBy =
            [ text " Judged as " ] ++ outcome ++ [ text " by ", displayNamedNodeLink "/user" judgedBy ]
    in
    [ h4 [] [ text diagnosis.node.name ]
    , ul [] <|
        displayListItems displayWager diagnosis.wagers
            ++ (case diagnosis.judgement of
                    Just judgementData ->
                        judgedAsBy [ strong [] [ displayOutcome judgementData.outcome ] ] judgementData.judgedBy
                            ++ [ text <| " at " ++ fromTime judgementData.timestamp
                               ]
                            |> li []
                            |> List.singleton

                    Nothing ->
                        case ( auth, caseDetail.state ) of
                            ( SignedIn _ user, Viewing ) ->
                                (if List.any (\wager -> wager.creator.id == user.node.id) diagnosis.wagers then
                                    []

                                 else
                                    [ button
                                        [ type_ "button", onClick <| changeWager "" ]
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
                                    ( inputValue, submitButtonDisabled, formAttributes ) =
                                        case newWager of
                                            Valid intValue val ->
                                                ( val, False, [ onSubmit <| SubmitWager caseDetail user.node.id diagnosis.node.id intValue ] )

                                            Invalid _ val ->
                                                ( val, True, [] )
                                in
                                if id == diagnosis.node.id then
                                    [ form formAttributes
                                        [ displayNamedNodeLink "/user" user.node
                                        , text " estimated "
                                        , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", value inputValue, onInput changeWager ] []
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
                ( submitButtonDisabled, formAttributes ) =
                    case ( prediction.diagnosis, prediction.confidence ) of
                        ( Valid diagnosis _, Valid confidence _ ) ->
                            ( False, [ onSubmit <| SubmitDiagnosis caseDetail user.node.id <| PredictionInput diagnosis confidence ] )

                        _ ->
                            ( True, [] )

                changePrediction newPrediction =
                    changeCase { caseDetail | state = AddingDiagnosis newPrediction }

                changeDiagnosis newValue =
                    changePrediction { prediction | diagnosis = validateDiagnosis newValue }

                changeConfidence newValue =
                    changePrediction { prediction | confidence = validateConfidence newValue }
            in
            [ form formAttributes
                [ h4 [] [ input [ type_ "text", placeholder "Diagnosis", value <| getInputValue prediction.diagnosis, onInput changeDiagnosis ] [] ]
                , ul []
                    [ li []
                        [ displayNamedNodeLink "/user" user.node
                        , text " estimated "
                        , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", value <| getInputValue prediction.confidence, onInput changeConfidence ] []
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
        changeComment : String -> Msg
        changeComment newComment =
            changeCase { caseDetail | state = AddingComment newComment }
    in
    case ( auth, caseDetail.state ) of
        ( SignedIn _ user, AddingComment string ) ->
            [ displayNamedNodeLink "/user" user.node
            , blockquote []
                [ form [ onSubmit <| SubmitComment caseDetail user.node.id string ]
                    [ input [ type_ "text", placeholder "Enter comment", onInput changeComment, value string ] []
                    , button [ type_ "submit", disabled << String.isEmpty << String.trim <| string ] [ text "Submit" ]
                    , cancelEditButton caseDetail
                    ]
                ]
            ]

        ( SignedIn _ _, Viewing ) ->
            [ button [ onClick <| changeComment "" ] [ text "Add comment" ] ]

        _ ->
            []


displayComment : CommentData -> List (Html msg)
displayComment comment =
    [ span [] [ displayNamedNodeLink "/user" comment.creator, text <| " at " ++ fromTime comment.timestamp ]
    , blockquote [] [ text comment.text ]
    ]


displayWager : WagerData -> List (Html msg)
displayWager wager =
    [ displayNamedNodeLink "/user" wager.creator
    , text " estimated "
    , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
    , text <| " at " ++ fromTime wager.timestamp
    ]


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


validateNonEmptyField : String -> InputValue -> FormField String
validateNonEmptyField fieldName inputValue =
    let
        trimmed =
            String.trim inputValue
    in
    inputValue
        |> (if String.isEmpty trimmed then
                Invalid (fieldName ++ " is required")

            else
                Valid trimmed
           )


changeNewReference : CaseData -> String -> Msg
changeNewReference caseData reference =
    ChangeNewCase { caseData | reference = validateReference reference }


validateDeadline : InputValue -> FormField Time.Posix
validateDeadline deadline =
    deadline
        |> (case Iso8601.toTime deadline of
                Ok value ->
                    Valid value

                _ ->
                    Invalid "Could not parse as ISO-8601 datetime string"
           )


changeNewDeadline : CaseData -> String -> Msg
changeNewDeadline caseData deadline =
    ChangeNewCase { caseData | deadline = validateDeadline deadline }


blankPrediction =
    PredictionData (validateDiagnosis "") (validateConfidence "")


addNewLine : CaseData -> Msg
addNewLine caseData =
    ChangeNewCase { caseData | predictions = caseData.predictions ++ [ blankPrediction ] }


changeNewPrediction : Int -> PredictionData -> CaseData -> Msg
changeNewPrediction index newPrediction caseData =
    ChangeNewCase { caseData | predictions = List.take index caseData.predictions ++ [ newPrediction ] ++ List.drop (index + 1) caseData.predictions }


validateDiagnosis =
    validateNonEmptyField "Diagnosis"


validateReference =
    validateNonEmptyField "Case reference"


changeNewDiagnosis : Int -> PredictionData -> CaseData -> String -> Msg
changeNewDiagnosis index oldPrediction caseData diagnosis =
    changeNewPrediction index { oldPrediction | diagnosis = validateDiagnosis diagnosis } caseData


validateConfidence : InputValue -> FormField Int
validateConfidence confidence =
    confidence
        |> (case String.toInt confidence of
                Just a ->
                    if a < 0 then
                        Invalid "Minimum confidence is 0%"

                    else if a > 100 then
                        Invalid "Maximum confidence is 100%"

                    else
                        Valid a

                Nothing ->
                    Invalid "Enter an integer from 0 to 100"
           )


changeNewConfidence : Int -> PredictionData -> CaseData -> String -> Msg
changeNewConfidence index oldPrediction caseData confidence =
    changeNewPrediction index { oldPrediction | confidence = validateConfidence confidence } caseData


changeNewPredictionRemove : Int -> CaseData -> Msg
changeNewPredictionRemove index caseData =
    ChangeNewCase { caseData | predictions = List.take index caseData.predictions ++ List.drop (index + 1) caseData.predictions }


displayPrediction : CaseData -> Int -> PredictionData -> Html Msg
displayPrediction caseData index prediction =
    fieldset []
        [ div []
            [ label [] [ text "Diagnosis: " ]
            , input
                [ required True
                , value <| getInputValue prediction.diagnosis
                , onInput (changeNewDiagnosis index prediction caseData)
                ]
                []
            , displayValidity prediction.diagnosis
            ]
        , div []
            [ label [] [ text "Confidence (%): " ]
            , input
                [ type_ "number"
                , step "1"
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , value <| getInputValue prediction.confidence
                , onInput (changeNewConfidence index prediction caseData)
                ]
                []
            , displayValidity prediction.confidence
            ]
        , button [ onClick (changeNewPredictionRemove index caseData), disabled <| List.length caseData.predictions == 1 ] [ text "➖" ]
        ]


printPrediction : PredictionInput -> Html Msg
printPrediction prediction =
    dl []
        [ dt [] [ text "Diagnosis" ]
        , dd [] [ text prediction.diagnosis ]
        , dt [] [ text "Confidence" ]
        , dd [] [ text <| String.fromInt prediction.confidence ++ "%" ]
        ]


getInputValue : FormField a -> InputValue
getInputValue field =
    case field of
        Valid _ value ->
            value

        Invalid _ value ->
            value


displayValidity : FormField a -> Html Msg
displayValidity field =
    case field of
        Valid _ _ ->
            span [ style "background-color" "green", style "color" "white" ] [ text "✓" ]

        Invalid validationMessage _ ->
            span [ style "background-color" "red", style "color" "white" ] [ text ("✗ " ++ validationMessage) ]


displayDebug : CaseInput -> Html Msg
displayDebug caseInput =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ text caseInput.reference ]
        , dt [] [ text "Deadline" ]
        , dd [] [ caseInput.deadline |> Iso8601.fromTime >> String.dropRight 5 |> text ]
        , dt [] [ text "Group" ]
        , dd []
            [ text <|
                case caseInput.groupId of
                    Present (Id id) ->
                        id

                    _ ->
                        "None"
            ]
        , dt [] [ text "Predictions" ]
        , dd [] [ caseInput.predictions |> List.map (printPrediction >> List.singleton >> li []) |> ul [] ]
        ]


displayOption : (Id -> Bool) -> NamedNodeData -> Html Msg
displayOption isSelected node =
    case node.id of
        Id id ->
            option [ value id, selected <| isSelected node.id ] [ text node.name ]


caseGroupChanged : CaseData -> Maybe Id -> Msg
caseGroupChanged caseData groupId =
    ChangeNewCase { caseData | groupId = groupId }


determineSelected : Maybe Id -> Id -> Bool
determineSelected selectedId id =
    selectedId == Just id


displayGroupSelect : Maybe Id -> List NamedNodeData -> (Maybe Id -> Msg) -> Html Msg
displayGroupSelect currentGroupId groups msgFn =
    let
        getNewGroupId string =
            case List.filter (\group -> group.id == Id string) groups of
                [ newGroup ] ->
                    Just newGroup.id

                _ ->
                    Nothing
    in
    select [ onInput <| msgFn << getNewGroupId ] <|
        List.map (determineSelected currentGroupId |> displayOption) <|
            NamedNodeData (Id "") "None"
                :: groups


displayNewForm : UserCandidate -> CaseData -> Html Msg
displayNewForm user caseData =
    div [] <|
        fieldset []
            [ div [] [ label [] [ text "Case reference: " ], input [ required True, value <| getInputValue caseData.reference, onInput (changeNewReference caseData) ] [], displayValidity caseData.reference ]
            , div [] [ label [] [ text "Deadline: " ], input [ required True, type_ "datetime-local", value <| getInputValue caseData.deadline, onInput (changeNewDeadline caseData) ] [], displayValidity caseData.deadline ]
            , div [] [ label [] [ text "Group: " ], displayGroupSelect caseData.groupId user.groups <| caseGroupChanged caseData ]
            ]
            :: List.indexedMap (displayPrediction caseData) caseData.predictions
            ++ [ fieldset [] [ button [ onClick (addNewLine caseData) ] [ text "➕" ] ] ]
            ++ (case prepareCaseInput caseData of
                    Just caseInput ->
                        [ displayDebug caseInput, button [ onClick <| SubmitCase user.node.id caseInput ] [ text "Submit" ] ]

                    _ ->
                        []
               )



-- Parse URL


type Route
    = Welcome
    | User Id
    | UserScore Id
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
                    , Query.user { id = userId } (User.scores <| mapToScore) |> makeRequest (ScoreDetail >> GotResponse)
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
                                    NewCase <|
                                        CaseData
                                            Nothing
                                            (validateReference "")
                                            [ blankPrediction ]
                                            (validateDeadline "")
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


preparePredictionInput : PredictionData -> Maybe PredictionInput
preparePredictionInput prediction =
    case prediction.diagnosis of
        Valid diagnosis _ ->
            case prediction.confidence of
                Valid confidence _ ->
                    Just <| PredictionInput diagnosis confidence

                Invalid _ _ ->
                    Nothing

        Invalid _ _ ->
            Nothing


prepareCaseInput : CaseData -> Maybe CaseInput
prepareCaseInput caseData =
    case caseData.reference of
        Valid reference _ ->
            case caseData.deadline of
                Valid deadline _ ->
                    let
                        predictions =
                            List.filterMap preparePredictionInput caseData.predictions
                    in
                    if List.isEmpty predictions || List.length predictions /= List.length caseData.predictions then
                        Nothing

                    else
                        Just <|
                            CaseInput
                                reference
                                (Id "")
                                (case caseData.groupId of
                                    Just id ->
                                        Present id

                                    Nothing ->
                                        Absent
                                )
                                deadline
                                predictions

                Invalid _ _ ->
                    Nothing

        Invalid _ _ ->
            Nothing


submitCase : CaseInput -> Cmd Msg
submitCase caseData =
    Mutation.addCase { caseInput = caseData }
        |> Graphql.Http.mutationRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> CaseCreated)


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserCandidate =
    { node : NamedNodeData
    , groups : List NamedNodeData
    }


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


type alias UserListResponse =
    List UserCandidate


userListQuery : SelectionSet UserListResponse RootQuery
userListQuery =
    Query.users <|
        SelectionSet.map2 UserCandidate
            (SelectionSet.map2 NamedNodeData User.id User.name)
            (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)


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


type alias ScoresListData =
    List Score


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



--
--type CaseOwnerGroup
--    = NotOwner NamedNodeData (Maybe NamedNodeData)
--    | OwnerViewing NamedNodeData (Maybe NamedNodeData) (List NamedNodeData)
--    | OwnerEditing NamedNodeData (Maybe NamedNodeData) (List NamedNodeData)


type CaseDetailState
    = Viewing
    | ChangingGroup (List NamedNodeData)
    | AddingDiagnosis PredictionData
    | AddingWager Id (FormField Int)
    | Judging Id
    | AddingComment String


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



--
--type Judgement
--    = Judged JudgementData
--    | NotJudged
--    | AddingJudgement
--    | AddingWager (FormField Int)
--


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
