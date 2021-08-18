module Helper exposing (GraphqlRemoteData, viewData)

import Dict exposing (Dict)
import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError exposing (GraphqlError, Location, PossiblyParsedData(..))
import Html exposing (Html, dd, div, dl, dt, li, pre, text, ul)
import Http
import Json.Decode as Decode
import List exposing (map)
import RemoteData exposing (RemoteData)
import String exposing (fromInt)


viewData : (a -> Html msg) -> GraphqlRemoteData a -> Html msg
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
                        ParsedData data ->
                            div [] [ displayData data, text "Http.GraphqlError with ParsedData" |> viewWithGraphqlErrors graphqlErrors ]

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

        RemoteData.Success data ->
            displayData data


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


type alias GraphqlRemoteData a =
    RemoteData (Graphql.Http.Error a) a
