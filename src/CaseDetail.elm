module CaseDetail exposing (Data, addComment, changeDeadline, changeGroup, fetchGroups, onChangeGroupResult, onCommentResult, onDeadlineResult, onGroupsResult, queryRequest, view)

import Common exposing (NamedNodeData, Now, caseUrl, displayListItems, displayNamedNode, displayTime, groupUrl, userUrl)
import Config exposing (api)
import Error
import FormField exposing (Field)
import Graphql.Http
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, b, blockquote, button, dd, div, dl, dt, form, input, li, option, select, text, ul)
import Html.Attributes exposing (disabled, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Predictions.Enum.Outcome
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
    | ChangingDeadline (Field Timestamp)
    | ChangingGroup (List NamedNodeData)
      --| AddingDiagnosis PredictionData
      --| AddingWager Id (Field Int)
      --| Judging Id
    | AddingComment (Field String) (Maybe (Graphql.Http.Error ()))


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
    { creator : NamedNodeData
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
    SelectionSet.map3 Wager
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


view : (Data -> msg) -> (Data -> msg) -> (Data -> Maybe Id -> msg) -> (Data -> Timestamp -> msg) -> (Data -> String -> msg) -> Now -> NamedNodeData -> Data -> Html msg
view dataUpdated fetchGroupsMsg changeGroupMsg changeDeadlineMsg addCommentMsg now currentUser (Data state case_) =
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
                        --[ div [] [ displayGroupSelect groupId groups <| SubmitGroup caseDetail ]
                        [ select [ onInput onChangeGroup ] <| option [ value private, selected (case_.group == Nothing) ] [ text privateGroupName ] :: List.map mapToOption groups
                        , div [] [ cancelEditButton ]
                        ]

                Viewing ->
                    div []
                        [ div [] [ viewGroup ]
                        , div [] [ button [ onClick << fetchGroupsMsg <| Data (ChangingGroup []) case_ ] [ text "Change" ] ]
                        ]

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
            [ text <| "The diagnosis is " ++ diagnosis.node.name ]

        --let
        --    judgedAsBy outcome judgedBy =
        --        [ text " Judged as " ] ++ outcome ++ [ text " by ", displayNamedNode userUrl judgedBy ]
        --in
        --[ h4 [] [ text diagnosis.node.name ]
        --, ul [] <|
        --    displayListItems (displayWager) diagnosis.wagers
        --        ++ (case diagnosis.judgement of
        --                Just judgementData ->
        --                    judgedAsBy [ strong [] [ text <| displayOutcome judgementData.outcome ] ] judgementData.judgedBy
        --                        ++ [ displayTime now judgementData.timestamp ]
        --                        |> li []
        --                        |> List.singleton
        --
        --                Nothing ->
        --                    let
        --                        changeWager : Field Int -> Msg
        --                        changeWager field =
        --                            changeCase { caseDetail | state = AddingWager diagnosis.node.id field }
        --                    in
        --                    case state of
        --                        Viewing ->
        --                            (if List.any (\wager -> wager.creator.id == user.node.id) diagnosis.wagers then
        --                                []
        --
        --                             else
        --                                [ button
        --                                    [ type_ "button", onClick <| changeWager <| FormField.newField validateConfidence ]
        --                                    [ text "Add wager" ]
        --                                ]
        --                            )
        --                                ++ [ button
        --                                        [ type_ "button", onClick <| changeCase { caseDetail | state = Judging diagnosis.node.id } ]
        --                                        [ text "Judge" ]
        --                                   ]
        --                                |> li []
        --                                |> List.singleton
        --
        --                        Judging id ->
        --                            let
        --                                judgeOutcomeButton outcome label =
        --                                    button [ type_ "button", onClick <| SubmitJudgement caseDetail user.node.id diagnosis.node.id outcome ] [ text label ]
        --                            in
        --                            if id == diagnosis.node.id then
        --                                judgedAsBy [ judgeOutcomeButton Right "Right", judgeOutcomeButton Wrong "Wrong", judgeOutcomeButton Indeterminate "Indeterminate" ]
        --                                    user.node
        --                                    ++ [ div [] [ cancelEditButton caseDetail ] ]
        --                                    |> li []
        --                                    |> List.singleton
        --
        --                            else
        --                                []
        --
        --                        AddingWager id newWager ->
        --                            let
        --                                ( submitButtonDisabled, formAttributes ) =
        --                                    case FormField.getValue newWager of
        --                                        Ok confidence ->
        --                                            ( False, [ onSubmit <| SubmitWager caseDetail user.node.id diagnosis.node.id confidence ] )
        --
        --                                        Err _ ->
        --                                            ( True, [] )
        --                            in
        --                            if id == diagnosis.node.id then
        --                                [ form formAttributes
        --                                    [ displayNamedNodeLink "/user" user.node
        --                                    , text " estimated "
        --                                    , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue newWager, FormField.onInput changeWager newWager ] []
        --                                    , text "%"
        --                                    , div []
        --                                        [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
        --                                        , cancelEditButton caseDetail
        --                                        ]
        --                                    ]
        --                                ]
        --                                    |> li []
        --                                    |> List.singleton
        --
        --                            else
        --                                []
        --
        --                        _ ->
        --                            []
        --           )
        --]
        displayNewDiagnosis : List (Html msg)
        displayNewDiagnosis =
            [ text "here is a new dx" ]

        --case state of
        --    Viewing ->
        --        [ button
        --            [ type_ "button", onClick <| changeCase { caseDetail | state = AddingDiagnosis blankPrediction } ]
        --            [ text "Add diagnosis" ]
        --        ]
        --            |> h4 []
        --            |> List.singleton
        --            |> li []
        --            |> List.singleton
        --
        --    AddingDiagnosis prediction ->
        --        let
        --            changePrediction newPrediction =
        --                changeCase { caseDetail | state = AddingDiagnosis newPrediction }
        --
        --            changeDiagnosis diagnosis =
        --                changePrediction { prediction | diagnosis = diagnosis }
        --
        --            changeConfidence confidence =
        --                changePrediction { prediction | confidence = confidence }
        --
        --            ( submitButtonDisabled, formAttributes ) =
        --                case ( FormField.getValue prediction.diagnosis, FormField.getValue prediction.confidence ) of
        --                    ( Ok diagnosis, Ok confidence ) ->
        --                        ( False, [ onSubmit <| SubmitDiagnosis caseDetail user.node.id <| PredictionInput diagnosis confidence Absent ] )
        --
        --                    _ ->
        --                        ( True, [] )
        --        in
        --        [ form formAttributes
        --            [ h4 [] [ input [ type_ "text", placeholder "Diagnosis", FormField.withValue prediction.diagnosis, FormField.onInput changeDiagnosis prediction.diagnosis ] [] ]
        --            , ul []
        --                [ li []
        --                    [ displayNamedNodeLink "/user" user.node
        --                    , text " estimated "
        --                    , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", FormField.withValue prediction.confidence, FormField.onInput changeConfidence prediction.confidence ] []
        --                    , text "%"
        --                    ]
        --                ]
        --            , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
        --            , cancelEditButton caseDetail
        --            ]
        --        ]
        --            |> li []
        --            |> List.singleton
        --
        --    _ ->
        --        []
        displayComment : Comment -> List (Html msg)
        displayComment comment =
            [ displayNamedNode userUrl comment.creator
            , displayTime now comment.timestamp
            , blockquote [] [ text comment.text ]
            ]

        displayNewComment : List (Html msg)
        displayNewComment =
            let
                changeComment : Field String -> msg
                changeComment comment =
                    changeState <| AddingComment comment Nothing
            in
            case state of
                AddingComment newComment error ->
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
                        ([ displayNamedNode userUrl currentUser
                         , blockquote []
                            [ form formAttributes
                                [ input [ type_ "text", placeholder "Enter comment", FormField.withValue newComment, FormField.onInput changeComment newComment ] []
                                , button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                , cancelEditButton
                                ]
                            ]
                         ]
                            ++ Maybe.withDefault [] (Maybe.map (Error.graphqlHttpErrorToHtml >> List.singleton >> div [] >> List.singleton) error)
                        )
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


fetchGroups : Id -> Graphql.Http.Request (List NamedNodeData)
fetchGroups userId =
    SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name
        |> Predictions.Object.User.groups
        |> Predictions.Query.user { id = userId }
        |> Graphql.Http.queryRequest api


onGroupsResult : Data -> Result (Graphql.Http.Error ()) (List NamedNodeData) -> Data
onGroupsResult (Data _ case_) result =
    case result of
        Ok groups ->
            Data (ChangingGroup groups) case_

        Err _ ->
            Data Viewing case_


changeGroup : Data -> Maybe Id -> Graphql.Http.Request (Maybe NamedNodeData)
changeGroup (Data _ case_) id =
    Predictions.Mutation.changeGroup
        (\args -> { args | newGroupId = Graphql.OptionalArgument.fromMaybeWithNull id })
        { caseId = case_.node.id }
        (SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name)
        |> Graphql.Http.mutationRequest api


onChangeGroupResult : Data -> Result (Graphql.Http.Error ()) (Maybe NamedNodeData) -> Data
onChangeGroupResult (Data _ case_) result =
    Data Viewing <|
        case result of
            Ok group ->
                { case_ | group = group }

            Err _ ->
                case_


newCommentInput : Field String
newCommentInput =
    FormField.newNonEmptyStringField "Comment"


addComment : Data -> String -> Graphql.Http.Request Comment
addComment (Data _ case_) text =
    Predictions.Mutation.addComment { caseId = case_.node.id, text = text } mapToComment
        |> Graphql.Http.mutationRequest api


onCommentResult : Data -> Result (Graphql.Http.Error ()) Comment -> Data
onCommentResult (Data state case_) result =
    case result of
        Ok comment ->
            Data Viewing { case_ | comments = case_.comments ++ [ comment ] }

        Err error ->
            Data
                (AddingComment
                    (case state of
                        AddingComment input _ ->
                            input

                        _ ->
                            newCommentInput
                    )
                 <|
                    Just error
                )
                case_


changeDeadline : Data -> Timestamp -> Graphql.Http.Request Timestamp
changeDeadline (Data _ case_) deadline =
    Predictions.Mutation.changeDeadline { caseId = case_.node.id, newDeadline = deadline }
        |> Graphql.Http.mutationRequest api


onDeadlineResult : Data -> Result (Graphql.Http.Error ()) Timestamp -> Data
onDeadlineResult (Data state case_) result =
    case result of
        Ok deadline ->
            Data Viewing { case_ | deadline = deadline }

        Err error ->
            Data
                (AddingComment
                    (case state of
                        AddingComment input _ ->
                            input

                        _ ->
                            newCommentInput
                    )
                 <|
                    Just error
                )
                case_
