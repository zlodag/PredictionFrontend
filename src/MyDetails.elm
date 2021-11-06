module MyDetails exposing (Data, queryRequest, view)

import Common exposing (NamedNodeData, Now, displayNamedNode, displayTime, userUrl)
import Config exposing (api)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html, a, dd, dl, dt, text)
import Html.Attributes exposing (href)
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import Round
import ScalarCodecs exposing (Timestamp)
import Url.Builder


type Data
    = Data MyDetails


type alias MyDetails =
    { node : NamedNodeData
    , created : Timestamp
    , score : Maybe Float
    }


queryRequest : Id -> Graphql.Http.Request Data
queryRequest userId =
    SelectionSet.map3 MyDetails
        (SelectionSet.map2 NamedNodeData Predictions.Object.User.id Predictions.Object.User.name)
        Predictions.Object.User.created
        (Predictions.Object.User.score { adjusted = False })
        |> SelectionSet.map Data
        |> Predictions.Query.user { id = userId }
        |> Graphql.Http.queryRequest api


view : Now -> Data -> Html msg
view now (Data myDetails) =
    dl []
        [ dt [] [ text "Me" ]
        , dd [] [ displayNamedNode userUrl myDetails.node ]
        , dt [] [ text "Created" ]
        , dd [] [ displayTime now myDetails.created ]
        , dt [] [ text "Score" ]
        , dd []
            [ case myDetails.score of
                Just score ->
                    a
                        [ href <|
                            Url.Builder.absolute
                                [ "users"
                                , case myDetails.node.id of
                                    Id id ->
                                        id
                                , "score"
                                ]
                                []
                        ]
                        [ text <| Round.round 4 score ]

                Nothing ->
                    text "No predictions judged yet"
            ]
        ]
