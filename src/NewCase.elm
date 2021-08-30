module NewCase exposing (Data, blankCase, displayNewForm)

import FormField exposing (Field)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Helper exposing (GraphqlRemoteData, NamedNodeData, PredictionData, UserCandidate, blankPrediction, displayGroupSelect, validateDeadline)
import Html exposing (Html, button, dd, div, dl, dt, fieldset, input, label, li, text, ul)
import Html.Attributes exposing (disabled, required, step, type_)
import Html.Events exposing (onClick)
import Iso8601
import Predictions.InputObject exposing (CaseInput, PredictionInput)
import Predictions.Scalar exposing (Id(..))
import ScalarCodecs exposing (Timestamp)


type alias CaseData =
    { groupId : Maybe Id
    , reference : Field String
    , predictions : List PredictionData
    , deadline : Field Timestamp
    }


type Data
    = Data CaseData


blankCase =
    CaseData
        Nothing
        (FormField.newNonEmptyStringField "Case reference")
        [ blankPrediction ]
        (FormField.newField validateDeadline)
        |> Data


displayPrediction : (CaseData -> msg) -> CaseData -> Int -> PredictionData -> Html msg
displayPrediction updateFunction caseData index prediction =
    let
        removeNewPrediction : msg
        removeNewPrediction =
            updateFunction { caseData | predictions = List.take index caseData.predictions ++ List.drop (index + 1) caseData.predictions }

        changeNewPrediction : PredictionData -> msg
        changeNewPrediction newPrediction =
            updateFunction { caseData | predictions = List.take index caseData.predictions ++ [ newPrediction ] ++ List.drop (index + 1) caseData.predictions }

        changeNewDiagnosis : Field String -> msg
        changeNewDiagnosis diagnosis =
            changeNewPrediction { prediction | diagnosis = diagnosis }

        changeNewConfidence : Field Int -> msg
        changeNewConfidence confidence =
            changeNewPrediction { prediction | confidence = confidence }
    in
    fieldset []
        [ div []
            [ label [] [ text "Diagnosis: " ]
            , input
                [ required True
                , FormField.withValue prediction.diagnosis
                , FormField.onInput changeNewDiagnosis prediction.diagnosis
                ]
                []
            , FormField.displayValidity prediction.diagnosis
            ]
        , div []
            [ label [] [ text "Confidence (%): " ]
            , input
                [ type_ "number"
                , step "1"
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , FormField.withValue prediction.confidence
                , FormField.onInput changeNewConfidence prediction.confidence
                ]
                []
            , FormField.displayValidity prediction.confidence
            ]
        , button [ onClick removeNewPrediction, disabled <| List.length caseData.predictions <= 1 ] [ text "➖" ]
        ]


preparePredictionInput : PredictionData -> Maybe PredictionInput
preparePredictionInput prediction =
    case ( FormField.getValue prediction.diagnosis, FormField.getValue prediction.confidence ) of
        ( Ok diagnosis, Ok confidence ) ->
            Just <| PredictionInput diagnosis confidence

        _ ->
            Nothing


prepareCaseInput : Id -> CaseData -> Maybe CaseInput
prepareCaseInput userId caseData =
    case ( FormField.getValue caseData.reference, FormField.getValue caseData.deadline ) of
        ( Ok reference, Ok deadline ) ->
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
                        userId
                        (case caseData.groupId of
                            Just id ->
                                Present id

                            Nothing ->
                                Absent
                        )
                        deadline
                        predictions

        _ ->
            Nothing


displayPredictionInput : PredictionInput -> Html msg
displayPredictionInput prediction =
    dl []
        [ dt [] [ text "Diagnosis" ]
        , dd [] [ text prediction.diagnosis ]
        , dt [] [ text "Confidence" ]
        , dd [] [ text <| String.fromInt prediction.confidence ++ "%" ]
        ]


displayCaseInput : CaseInput -> Html msg
displayCaseInput caseInput =
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
        , dd [] [ caseInput.predictions |> List.map (displayPredictionInput >> List.singleton >> li []) |> ul [] ]
        ]


displayNewForm : (Data -> msg) -> (CaseInput -> msg) -> UserCandidate -> Data -> Html msg
displayNewForm updateFunction submitFunction user data =
    let
        updateCase : CaseData -> msg
        updateCase newCaseData =
            updateFunction <| Data newCaseData

        caseData =
            case data of
                Data d ->
                    d

        changeNewReference : Field String -> msg
        changeNewReference reference =
            updateCase { caseData | reference = reference }

        changeNewDeadline : Field Timestamp -> msg
        changeNewDeadline deadline =
            updateCase { caseData | deadline = deadline }

        changeNewGroup : Maybe Id -> msg
        changeNewGroup groupId =
            updateCase { caseData | groupId = groupId }

        addNewLine : msg
        addNewLine =
            updateCase { caseData | predictions = caseData.predictions ++ [ blankPrediction ] }
    in
    div [] <|
        fieldset []
            [ div [] [ label [] [ text "Case reference: " ], input [ required True, FormField.withValue caseData.reference, FormField.onInput changeNewReference caseData.reference ] [], FormField.displayValidity caseData.reference ]
            , div [] [ label [] [ text "Deadline: " ], input [ required True, type_ "datetime-local", FormField.withValue caseData.deadline, FormField.onInput changeNewDeadline caseData.deadline ] [], FormField.displayValidity caseData.deadline ]
            , div [] [ label [] [ text "Group: " ], displayGroupSelect caseData.groupId user.groups changeNewGroup ]
            ]
            :: List.indexedMap (displayPrediction updateCase caseData) caseData.predictions
            ++ [ fieldset [] [ button [ onClick addNewLine ] [ text "➕" ] ] ]
            ++ (case prepareCaseInput user.node.id caseData of
                    Just caseInput ->
                        [ displayCaseInput caseInput, button [ onClick <| submitFunction caseInput ] [ text "Submit" ] ]

                    _ ->
                        []
               )
