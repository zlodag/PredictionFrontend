module UserScore exposing (Data, queryRequest, view)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Common exposing (NamedNodeData, Now, UserInfo, caseUrl, displayNamedNode, displayOutcomeSymbol, displayTime, getShortDateString)
import Config exposing (api)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Object.Score
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import Round
import ScalarCodecs exposing (Timestamp)
import Svg as S
import Time


type Data
    = Data Hovering (List Score)


type alias Hovering =
    List (CI.One Score CI.Dot)


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


queryRequest : Id -> Graphql.Http.Request Data
queryRequest userId =
    SelectionSet.map8 Score
        Predictions.Object.Score.judged
        (SelectionSet.map2 NamedNodeData Predictions.Object.Score.caseId Predictions.Object.Score.reference)
        Predictions.Object.Score.diagnosis
        Predictions.Object.Score.confidence
        Predictions.Object.Score.outcome
        Predictions.Object.Score.brierScore
        Predictions.Object.Score.averageBrierScore
        Predictions.Object.Score.adjustedBrierScore
        |> Predictions.Object.User.scores
        |> SelectionSet.map (Data [])
        |> Predictions.Query.user { id = userId }
        |> Graphql.Http.queryRequest api


view : (Data -> msg) -> Now -> Data -> Html msg
view dataUpdated now (Data hovering scores) =
    let
        onHover : Hovering -> msg
        onHover newHovering =
            Data newHovering scores |> dataUpdated
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


displayChart : Time.Zone -> (Hovering -> msg) -> Hovering -> List Score -> Html msg
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


displayConfidenceHistogram : List Score -> Html msg
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
