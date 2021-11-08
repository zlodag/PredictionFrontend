module CaseDetail exposing (Data, addComment, changeDeadline, changeGroup, fetchGroups, judgeOutcome, onAddCommentResult, onChangeDeadlineResult, onChangeGroupResult, onFetchGroupsResult, onJudgeOutcomeResult, onSubmitDiagnosisResult, onSubmitWagerResult, queryRequest, submitDiagnosis, submitWager, view)

import Common exposing (NamedNodeData, Now, caseUrl, displayListItems, displayNamedNode, displayOutcome, displayTime, groupUrl, userUrl)
import Config exposing (api)
import FormField
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, b, blockquote, button, dd, div, dl, dt, form, h4, input, li, option, select, strong, text, ul)
import Html.Attributes exposing (disabled, placeholder, selected, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Mutation
import Predictions.Object
import Predictions.Object.Case
import Predictions.Object.Comment
import Predictions.Object.Diagnosis
import Predictions.Object.Group
import Predictions.Object.Judgement
import Predictions.Object.User
import Predictions.Object.Wager
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import ScalarCodecs exposing (Id, Timestamp)


type Data
    = Data State Case


type State
    = Viewing
    | ChangingDeadline (FormField.Field Timestamp)
    | ChangingGroup (List NamedNodeData)
    | AddingDiagnosis (FormField.Field String) (FormField.Field Int)
    | AddingWager Id (FormField.Field Int)
    | Judging Id
    | AddingComment (FormField.Field String)


type alias Case =
    { node : NamedNodeData
    , creator : NamedNodeData
    , group : Maybe NamedNodeData
    , deadline : Timestamp
    , diagnoses : List Diagnosis
    , comments : List Comment
    , tags : List String
    }


type alias Diagnosis =
    { node : NamedNodeData
    , wagers : List Wager
    , judgement : Maybe Judgement
    }


type alias Wager =
    { id : Id
    , creator : NamedNodeData
    , confidence : Int
    , timestamp : Timestamp
    }


type alias Judgement =
    { judgedBy : NamedNodeData
    , timestamp : Timestamp
    , outcome : Predictions.Enum.Outcome.Outcome
    }


type alias Comment =
    { creator : NamedNodeData
    , timestamp : Timestamp
    , text : String
    }


queryRequest : Id -> Graphql.Http.Request Data
queryRequest caseId =
    SelectionSet.map (Data Viewing) mapToCase
        |> Predictions.Query.case_ { id = caseId }
        |> Graphql.Http.queryRequest api


mapToCase : SelectionSet Case Predictions.Object.Case
mapToCase =
    SelectionSet.map7 Case
        (SelectionSet.map2 NamedNodeData Predictions.Object.Case.id Predictions.Object.Case.reference)
        (Predictions.Object.Case.creator <| SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
        (Predictions.Object.Case.group <| SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
        Predictions.Object.Case.deadline
        (Predictions.Object.Case.diagnoses <| mapToDiagnosis)
        (Predictions.Object.Case.comments <| mapToComment)
        Predictions.Object.Case.tags


mapToDiagnosis : SelectionSet Diagnosis Predictions.Object.Diagnosis
mapToDiagnosis =
    SelectionSet.map3 Diagnosis
        (SelectionSet.map2 NamedNodeData Predictions.Object.Diagnosis.id Predictions.Object.Diagnosis.name)
        (Predictions.Object.Diagnosis.wagers <| mapToWager)
        (Predictions.Object.Diagnosis.judgement <| mapToJudgement)


mapToWager : SelectionSet Wager Predictions.Object.Wager
mapToWager =
    SelectionSet.map4 Wager
        Predictions.Object.Wager.id
        (Predictions.Object.Wager.creator <| SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
        Predictions.Object.Wager.confidence
        Predictions.Object.Wager.timestamp


mapToJudgement : SelectionSet Judgement Predictions.Object.Judgement
mapToJudgement =
    SelectionSet.map3 Judgement
        (Predictions.Object.Judgement.judgedBy <| SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
        Predictions.Object.Judgement.timestamp
        Predictions.Object.Judgement.outcome


mapToComment : SelectionSet Comment Predictions.Object.Comment
mapToComment =
    SelectionSet.map3 Comment
        (Predictions.Object.Comment.creator <| SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
        Predictions.Object.Comment.timestamp
        Predictions.Object.Comment.text


view :
    (Data -> msg)
    -> (Data -> msg)
    -> (Data -> Maybe Id -> msg)
    -> (Data -> Timestamp -> msg)
    -> (Data -> String -> Int -> msg)
    -> (Data -> Id -> Int -> msg)
    -> (Data -> Id -> Outcome -> msg)
    -> (Data -> String -> msg)
    -> Now
    -> NamedNodeData
    -> Data
    -> Html msg
view dataUpdated fetchGroupsMsg changeGroupMsg changeDeadlineMsg submitDiagnosisMsg submitWagerMsg judgeOutcomeMsg addCommentMsg now currentUser (Data state case_) =
    let
        changeState s =
            Data s case_ |> dataUpdated

        cancelEditButton =
            button [ type_ "button", onClick <| changeState Viewing ] [ text "Cancel" ]

        viewDeadline =
            displayTime now case_.deadline

        displayDeadline : Html msg
        displayDeadline =
            case state of
                ChangingDeadline newDeadline ->
                    let
                        ( submitButtonDisabled, formAttributes ) =
                            case FormField.getValue newDeadline of
                                Ok deadline ->
                                    ( False, [ onSubmit <| changeDeadlineMsg (Data state case_) deadline ] )

                                Err _ ->
                                    ( True, [] )
                    in
                    form formAttributes
                        [ input [ type_ "datetime-local", FormField.withValue newDeadline, FormField.onInput (ChangingDeadline >> changeState) newDeadline ] []
                        , FormField.displayValidity newDeadline
                        , div []
                            [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                            , cancelEditButton
                            ]
                        ]

                Viewing ->
                    div []
                        [ div [] [ viewDeadline ]
                        , div [] [ button [ onClick <| changeState <| ChangingDeadline <| FormField.newField FormField.validateDeadline ] [ text "Change" ] ]
                        ]

                _ ->
                    viewDeadline

        privateGroupName =
            "(Private)"

        viewGroup =
            case_.group |> Maybe.map (displayNamedNode groupUrl) |> Maybe.withDefault (text privateGroupName)

        displayCaseGroup =
            case state of
                ChangingGroup groups ->
                    let
                        private =
                            "_private"

                        onChangeGroup : String -> msg
                        onChangeGroup value =
                            changeGroupMsg (Data state case_) <|
                                if value == private then
                                    Nothing

                                else
                                    Just <| Id value

                        mapToOption : NamedNodeData -> Html msg
                        mapToOption group =
                            case group.id of
                                Id groupId ->
                                    let
                                        s =
                                            case case_.group of
                                                Just g ->
                                                    case g.id of
                                                        Id id ->
                                                            id == groupId

                                                Nothing ->
                                                    False
                                    in
                                    option [ value groupId, selected s ] [ text group.name ]
                    in
                    div []
                        [ select [ onInput onChangeGroup ] <| option [ value private, selected (case_.group == Nothing) ] [ text privateGroupName ] :: List.map mapToOption groups
                        , div [] [ cancelEditButton ]
                        ]

                Viewing ->
                    if currentUser.id == case_.creator.id then
                        div []
                            [ div [] [ viewGroup ]
                            , div [] [ button [ onClick << fetchGroupsMsg <| Data (ChangingGroup []) case_ ] [ text "Change" ] ]
                            ]

                    else
                        viewGroup

                _ ->
                    viewGroup

        displayWager : Wager -> List (Html msg)
        displayWager wager =
            [ displayNamedNode userUrl wager.creator
            , text " estimated "
            , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
            , displayTime now wager.timestamp
            ]

        displayDiagnosis : Diagnosis -> List (Html msg)
        displayDiagnosis diagnosis =
            let
                judgedAsBy outcome judgedBy =
                    [ text " Judged as " ] ++ outcome ++ [ text " by ", displayNamedNode userUrl judgedBy ]
            in
            [ h4 [] [ text diagnosis.node.name ]
            , ul [] <|
                displayListItems displayWager diagnosis.wagers
                    ++ (case diagnosis.judgement of
                            Just judgementData ->
                                judgedAsBy [ strong [] [ text <| displayOutcome judgementData.outcome ] ] judgementData.judgedBy
                                    ++ [ displayTime now judgementData.timestamp ]
                                    |> li []
                                    |> List.singleton

                            Nothing ->
                                let
                                    changeWager : FormField.Field Int -> msg
                                    changeWager field =
                                        changeState <| AddingWager diagnosis.node.id field
                                in
                                case state of
                                    Viewing ->
                                        (if List.any (\wager -> wager.creator.id == currentUser.id) diagnosis.wagers then
                                            []

                                         else
                                            [ button
                                                [ type_ "button", onClick <| changeWager <| FormField.newField FormField.validateConfidence ]
                                                [ text "Add wager" ]
                                            ]
                                        )
                                            ++ [ button
                                                    [ type_ "button", onClick <| changeState <| Judging diagnosis.node.id ]
                                                    [ text "Judge" ]
                                               ]
                                            |> li []
                                            |> List.singleton

                                    Judging id ->
                                        if id == diagnosis.node.id then
                                            let
                                                judgeOutcomeButton outcome label =
                                                    button [ type_ "button", onClick <| judgeOutcomeMsg (Data state case_) diagnosis.node.id outcome ] [ text label ]
                                            in
                                            judgedAsBy [ judgeOutcomeButton Right "Right", judgeOutcomeButton Wrong "Wrong", judgeOutcomeButton Indeterminate "Indeterminate" ]
                                                currentUser
                                                ++ [ div [] [ cancelEditButton ] ]
                                                |> li []
                                                |> List.singleton

                                        else
                                            []

                                    AddingWager id newWager ->
                                        if id == diagnosis.node.id then
                                            let
                                                ( submitButtonDisabled, formAttributes ) =
                                                    case FormField.getValue newWager of
                                                        Ok confidence ->
                                                            ( False, [ onSubmit <| submitWagerMsg (Data state case_) diagnosis.node.id confidence ] )

                                                        Err _ ->
                                                            ( True, [] )
                                            in
                                            [ form formAttributes
                                                [ displayNamedNode userUrl currentUser
                                                , text " estimated "
                                                , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue newWager, FormField.onInput changeWager newWager ] []
                                                , text "%"
                                                , div []
                                                    [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                                    , cancelEditButton
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

        displayNewDiagnosis : List (Html msg)
        displayNewDiagnosis =
            case state of
                Viewing ->
                    [ button
                        [ type_ "button", onClick <| changeState <| AddingDiagnosis (FormField.newNonEmptyStringField "Diagnosis") (FormField.newField FormField.validateConfidence) ]
                        [ text "Add diagnosis" ]
                    ]
                        |> h4 []
                        |> List.singleton
                        |> li []
                        |> List.singleton

                AddingDiagnosis diagnosis confidence ->
                    let
                        changeDiagnosis newDiagnosis =
                            changeState <| AddingDiagnosis newDiagnosis confidence

                        changeConfidence newConfidence =
                            changeState <| AddingDiagnosis diagnosis newConfidence

                        ( submitButtonDisabled, formAttributes ) =
                            case ( FormField.getValue diagnosis, FormField.getValue confidence ) of
                                ( Ok d, Ok c ) ->
                                    ( False, [ onSubmit <| submitDiagnosisMsg (Data state case_) d c ] )

                                _ ->
                                    ( True, [] )
                    in
                    [ form formAttributes
                        [ h4 [] [ input [ type_ "text", placeholder "Diagnosis", FormField.withValue diagnosis, FormField.onInput changeDiagnosis diagnosis ] [] ]
                        , ul []
                            [ li []
                                [ displayNamedNode userUrl currentUser
                                , text " estimated "
                                , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue confidence, FormField.onInput changeConfidence confidence ] []
                                , text "%"
                                ]
                            ]
                        , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                        , cancelEditButton
                        ]
                    ]
                        |> li []
                        |> List.singleton

                _ ->
                    []

        displayComment : Comment -> List (Html msg)
        displayComment comment =
            [ displayNamedNode userUrl comment.creator
            , displayTime now comment.timestamp
            , blockquote [] [ text comment.text ]
            ]

        displayNewComment : List (Html msg)
        displayNewComment =
            let
                changeComment : FormField.Field String -> msg
                changeComment comment =
                    changeState <| AddingComment comment
            in
            case state of
                AddingComment newComment ->
                    let
                        ( submitButtonDisabled, formAttributes ) =
                            case FormField.getValue newComment of
                                Ok comment ->
                                    ( False
                                    , [ onSubmit <| addCommentMsg (Data state case_) comment ]
                                    )

                                Err _ ->
                                    ( True, [] )
                    in
                    [ li []
                        [ displayNamedNode userUrl currentUser
                        , blockquote []
                            [ form formAttributes
                                [ input [ type_ "text", placeholder "Enter comment", FormField.withValue newComment, FormField.onInput changeComment newComment ] []
                                , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                , cancelEditButton
                                ]
                            ]
                        ]
                    ]

                Viewing ->
                    [ li [] [ button [ onClick <| changeComment <| newCommentInput ] [ text "Add comment" ] ] ]

                _ ->
                    []
    in
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNode caseUrl case_.node ]
        , dt [] [ text "Deadline" ]
        , dd [] [ displayDeadline ]
        , dt [] [ text "Creator" ]
        , dd [] [ displayNamedNode userUrl case_.creator ]
        , dt [] [ text "Group" ]
        , dd [] [ displayCaseGroup ]
        , dt [] [ text <| "Diagnoses (" ++ String.fromInt (List.length case_.diagnoses) ++ ")" ]
        , dd [] [ ul [] <| displayListItems displayDiagnosis case_.diagnoses ++ displayNewDiagnosis ]
        , dt [] [ text <| "Comments (" ++ String.fromInt (List.length case_.comments) ++ ")" ]
        , dd []
            [ ul [] <|
                displayListItems displayComment case_.comments
                    ++ displayNewComment
            ]
        , dt [] [ text <| "Tags (" ++ String.fromInt (List.length case_.tags) ++ ")" ]
        , dd [] [ ul [] <| List.map (text >> List.singleton >> li []) case_.tags ]
        ]


onResult : (a -> Case -> Data) -> Data -> Result (Graphql.Http.Error ()) a -> Data
onResult updateCase (Data _ case_) result =
    case result of
        Ok a ->
            case_ |> updateCase a

        Err _ ->
            Data Viewing case_


onDiagnosisRelatedResult : (a -> Diagnosis -> Diagnosis) -> Id -> Data -> Result (Graphql.Http.Error ()) a -> Data
onDiagnosisRelatedResult updateFn diagnosisId (Data _ case_) result =
    Data Viewing <|
        case result of
            Ok a ->
                { case_
                    | diagnoses =
                        List.map
                            (\d ->
                                if d.node.id == diagnosisId then
                                    updateFn a d

                                else
                                    d
                            )
                            case_.diagnoses
                }

            Err _ ->
                case_


fetchGroups : Id -> Graphql.Http.Request (List NamedNodeData)
fetchGroups userId =
    SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name
        |> Predictions.Object.User.groups
        |> Predictions.Query.user { id = userId }
        |> Graphql.Http.queryRequest api


onFetchGroupsResult : Data -> Result (Graphql.Http.Error ()) (List NamedNodeData) -> Data
onFetchGroupsResult =
    onResult (\groups case_ -> Data (ChangingGroup groups) case_)


changeGroup : Data -> Maybe Id -> Graphql.Http.Request (Maybe NamedNodeData)
changeGroup (Data _ case_) id =
    Predictions.Mutation.changeGroup
        (\args -> { args | newGroupId = Graphql.OptionalArgument.fromMaybeWithNull id })
        { caseId = case_.node.id }
        (SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
        |> Graphql.Http.mutationRequest api


onChangeGroupResult : Data -> Result (Graphql.Http.Error ()) (Maybe NamedNodeData) -> Data
onChangeGroupResult =
    onResult (\group case_ -> Data Viewing { case_ | group = group })


changeDeadline : Data -> Timestamp -> Graphql.Http.Request Timestamp
changeDeadline (Data _ case_) deadline =
    Predictions.Mutation.changeDeadline { caseId = case_.node.id, newDeadline = deadline }
        |> Graphql.Http.mutationRequest api


onChangeDeadlineResult : Data -> Result (Graphql.Http.Error ()) Timestamp -> Data
onChangeDeadlineResult =
    onResult (\deadline case_ -> Data Viewing { case_ | deadline = deadline })


submitDiagnosis : Data -> String -> Int -> Graphql.Http.Request Diagnosis
submitDiagnosis (Data _ case_) diagnosis confidence =
    Predictions.Mutation.addDiagnosis
        { caseId = case_.node.id
        , prediction =
            { diagnosis = diagnosis
            , confidence = confidence
            , outcome = Absent
            }
        }
        mapToDiagnosis
        |> Graphql.Http.mutationRequest api


onSubmitDiagnosisResult : Data -> Result (Graphql.Http.Error ()) Diagnosis -> Data
onSubmitDiagnosisResult =
    onResult (\diagnosis case_ -> Data Viewing { case_ | diagnoses = case_.diagnoses ++ [ diagnosis ] })


submitWager : Id -> Int -> Graphql.Http.Request Wager
submitWager diagnosisId confidence =
    Predictions.Mutation.addWager
        { diagnosisId = diagnosisId
        , confidence = confidence
        }
        mapToWager
        |> Graphql.Http.mutationRequest api


onSubmitWagerResult : Id -> Data -> Result (Graphql.Http.Error ()) Wager -> Data
onSubmitWagerResult =
    onDiagnosisRelatedResult (\wager diagnosis -> { diagnosis | wagers = diagnosis.wagers ++ [ wager ] })


judgeOutcome : Id -> Outcome -> Graphql.Http.Request Judgement
judgeOutcome diagnosisId outcome =
    Predictions.Mutation.judgeOutcome
        { diagnosisId = diagnosisId
        , outcome = outcome
        }
        mapToJudgement
        |> Graphql.Http.mutationRequest api


onJudgeOutcomeResult : Id -> Data -> Result (Graphql.Http.Error ()) Judgement -> Data
onJudgeOutcomeResult =
    onDiagnosisRelatedResult (\judgement diagnosis -> { diagnosis | judgement = Just judgement })


addComment : Data -> String -> Graphql.Http.Request Comment
addComment (Data _ case_) text =
    Predictions.Mutation.addComment { caseId = case_.node.id, text = text } mapToComment
        |> Graphql.Http.mutationRequest api


onAddCommentResult : Data -> Result (Graphql.Http.Error ()) Comment -> Data
onAddCommentResult =
    onResult (\comment case_ -> Data Viewing { case_ | comments = case_.comments ++ [ comment ] })


newCommentInput : FormField.Field String
newCommentInput =
    FormField.newNonEmptyStringField "Comment"
