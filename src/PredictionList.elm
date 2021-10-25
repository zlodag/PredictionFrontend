module PredictionList exposing (Params, Predictions, display, fetch, initial)

import Config exposing (graphQlEndpoint)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, NamedNodeData, Now, displayNamedNodeLink, displayOutcomeSymbol, displayTime)
import Html exposing (Html, div, input, label, option, select, table, td, text, th, tr)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onInput)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Object.Prediction
import Predictions.Query exposing (PredictionsOptionalArguments)
import Predictions.Scalar exposing (Id)
import RemoteData exposing (RemoteData(..))
import ScalarCodecs exposing (Timestamp)


type alias Params =
    { creatorId : Id
    , outcome : OptionalArgument Outcome
    , diagnosisFilter : String
    }


type alias Predictions =
    GraphqlRemoteData (List Prediction)


type alias Prediction =
    { case_ : NamedNodeData
    , diagnosis : String
    , outcome : Maybe Outcome
    , created : Timestamp
    }


initial : Id -> Params
initial creatorId =
    Params creatorId Absent ""


display : Now -> (Params -> Predictions -> msg) -> (Params -> msg) -> Params -> Predictions -> Html msg
display now updateModel fetchPredictions params predictions =
    let
        onDiagnosisFilter : String -> msg
        onDiagnosisFilter diagnosisFilter =
            updateModel { params | diagnosisFilter = diagnosisFilter } predictions

        onOutcomeChanged : String -> msg
        onOutcomeChanged outcome =
            { params
                | outcome =
                    case outcome of
                        "null" ->
                            Null

                        "right" ->
                            Present Right

                        "wrong" ->
                            Present Wrong

                        "indeterminate" ->
                            Present Indeterminate

                        _ ->
                            Absent
            }
                |> fetchPredictions
    in
    div [] <|
        [ div []
            [ label [] [ text "Outcome: " ]
            , select [ onInput onOutcomeChanged ]
                [ option [ value "all", selected (params.outcome == Absent) ] [ text "All" ]
                , option [ value "null", selected (params.outcome == Null) ] [ text "Not judged" ]
                , option [ value "right", selected (params.outcome == Present Right) ] [ text "Right" ]
                , option [ value "wrong", selected (params.outcome == Present Wrong) ] [ text "Wrong" ]
                , option [ value "indeterminate", selected (params.outcome == Present Indeterminate) ] [ text "Indeterminate" ]
                ]
            ]
        , div []
            [ label [] [ text "Filter diagnoses: " ]
            , input [ type_ "text", value params.diagnosisFilter, onInput onDiagnosisFilter ] []
            ]
        ]
            ++ displayResult now (String.toLower params.diagnosisFilter) predictions


displayResult : Now -> String -> Predictions -> List (Html msg)
displayResult now diagnosisFilter predictions =
    case predictions of
        Success data ->
            let
                diagnoses : List Prediction
                diagnoses =
                    if String.isEmpty diagnosisFilter then
                        data

                    else
                        List.filter (\prediction -> String.toLower prediction.diagnosis |> String.contains diagnosisFilter) data
            in
            [ table [] <|
                ([ "Timestamp", "Case", "", "Diagnosis" ] |> List.map (text >> List.singleton >> th []) |> tr [])
                    :: List.map (displayPrediction now) diagnoses
            ]

        _ ->
            []


displayPrediction : Now -> Prediction -> Html msg
displayPrediction now prediction =
    tr []
        [ td [] [ displayTime now prediction.created ]
        , td [] [ displayNamedNodeLink [ "case" ] prediction.case_ ]
        , td [] [ Maybe.map displayOutcomeSymbol prediction.outcome |> Maybe.withDefault "..." |> text ]
        , td [] [ text prediction.diagnosis ]
        ]


predictionDecoder =
    SelectionSet.map4 Prediction
        (SelectionSet.map2 NamedNodeData Predictions.Object.Prediction.caseId Predictions.Object.Prediction.caseReference)
        Predictions.Object.Prediction.diagnosis
        Predictions.Object.Prediction.outcome
        Predictions.Object.Prediction.timestamp


fetch : (Params -> Predictions -> msg) -> Params -> Cmd msg
fetch updateModel params =
    let
        optionalArgs : PredictionsOptionalArguments -> PredictionsOptionalArguments
        optionalArgs args =
            { args | outcome = params.outcome }
    in
    Predictions.Query.predictions optionalArgs { creatorId = params.creatorId } predictionDecoder
        |> Graphql.Http.queryRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> updateModel params)
