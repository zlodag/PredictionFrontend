module Helper exposing (viewData, toTime)

import Dict exposing (Dict)
import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError exposing (GraphqlError, Location, PossiblyParsedData(..))
import Html exposing (Html, dd, div, dl, dt, li, pre, text, ul)
import Http
import Iso8601 as Iso
import Json.Decode as Decode
import List exposing (map)
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String exposing (fromInt, toInt)
import Time exposing (Posix)


viewData : (a -> Html msg) -> RemoteData (Graphql.Http.Error (Maybe a)) (Maybe a) -> Html msg
viewData displayData remoteData =
    case remoteData of
        RemoteData.Loading ->
            text "Loading"

        RemoteData.NotAsked ->
            text "NotAsked"

        RemoteData.Failure e ->
            case e of
                Graphql.Http.GraphqlError possiblyParsedData graphqlErrors ->
                    case possiblyParsedData of
                        ParsedData d ->
                            case d of
                                Just data ->
                                    div [] [ displayData data, text "Http.GraphqlError with ParsedData" |> viewWithGraphqlErrors graphqlErrors ]

                                Nothing ->
                                    text "Http.GraphqlError with empty ParsedData" |> viewWithGraphqlErrors graphqlErrors

                        UnparsedData _ ->
                            text "Http.GraphqlError with UnparsedData" |> viewWithGraphqlErrors graphqlErrors

                Graphql.Http.HttpError httpError ->
                    case httpError of
                        BadUrl url ->
                            text <| "Graphql.Http.HttpError: BadUrl: " ++ url

                        Timeout ->
                            text "Graphql.Http.HttpError: Timeout"

                        NetworkError ->
                            text "Graphql.Http.HttpError: NetworkError"

                        BadStatus metadata string ->
                            div [] [ text <| "Graphql.Http.HttpError: BadStatus: " ++ string, viewMetadata metadata ]

                        BadPayload error ->
                            div [] [ text "Graphql.Http.HttpError: BadPayload", pre [] [ text <| Decode.errorToString error ] ]

        RemoteData.Success d ->
            case d of
                Just data ->
                    displayData data

                Nothing ->
                    text "RemoteData.Success with empty ParsedData"


viewHeaders : Dict String String -> List (Html msg)
viewHeaders headers =
    Dict.foldl addHeader [] headers


addHeader : String -> String -> List (Html msg) -> List (Html msg)
addHeader key value headers =
    headers ++ [ dt [] [ text key ], dd [] [ text value ] ]


viewMetadata : Http.Metadata -> Html msg
viewMetadata metadata =
    dl []
        [ dt [] [ text "url" ]
        , dd [] [ text metadata.url ]
        , dt [] [ text "statusCode" ]
        , dd [] [ text <| fromInt metadata.statusCode ]
        , dt [] [ text "statusText" ]
        , dd [] [ text metadata.statusText ]
        , dt [] [ text "headers" ]
        , dd [] (viewHeaders metadata.headers)
        ]


viewGraphqlErrorLocation : Location -> Html msg
viewGraphqlErrorLocation location =
    dl []
        [ dt [] [ text "line" ]
        , dd [] [ text <| fromInt location.line ]
        , dt [] [ text "column" ]
        , dd [] [ text <| fromInt location.column ]
        ]


viewGraphqlError : GraphqlError -> Html msg
viewGraphqlError error =
    li []
        [ dl []
            [ dt [] [ text "error.message" ]
            , dd [] [ text error.message ]
            , dt [] [ text "error.locations" ]
            , dd [] (Maybe.withDefault [] error.locations |> map viewGraphqlErrorLocation)
            ]
        ]


viewWithGraphqlErrors : List GraphqlError -> Html msg -> Html msg
viewWithGraphqlErrors errors msg =
    div []
        [ msg
        , ul [] (errors |> map viewGraphqlError)
        ]


toTime : Timestamp -> String
toTime timestamp =
    case timestamp of
        Timestamp string ->
            case string |> toInt of
                Just millis ->
                    millis |> Time.millisToPosix |> Iso.fromTime

                Nothing ->
                    "Non-integer timestamp"
