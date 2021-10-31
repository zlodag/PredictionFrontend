module Error exposing (graphqlHttpErrorToHtml, responseToHtml)

import Dict
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (Html, dd, div, dl, dt, li, pre, text, ul)
import Http
import Json.Decode


responseToHtml : Http.Response String -> Html msg
responseToHtml r =
    case r of
        Http.BadUrl_ url ->
            badUrl url

        Http.Timeout_ ->
            timeout

        Http.NetworkError_ ->
            networkError

        Http.BadStatus_ metadata body ->
            badStatus metadata body

        Http.GoodStatus_ metadata body ->
            goodStatus metadata body


graphqlHttpErrorToHtml : Graphql.Http.Error a -> Html msg
graphqlHttpErrorToHtml e =
    case e of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            div []
                [ text "GraphqlError"
                , ul [] (graphqlErrors |> List.map viewGraphqlError)
                ]

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    badUrl url

                Graphql.Http.Timeout ->
                    timeout

                Graphql.Http.NetworkError ->
                    networkError

                Graphql.Http.BadStatus metadata body ->
                    badStatus metadata body

                Graphql.Http.BadPayload error ->
                    badPayload error


viewGraphqlError : Graphql.Http.GraphqlError.GraphqlError -> Html msg
viewGraphqlError error =
    li []
        [ dl []
            [ dt [] [ text "error.message" ]
            , dd [] [ text error.message ]
            , dt [] [ text "error.locations" ]
            , dd [] (Maybe.withDefault [] error.locations |> List.map viewGraphqlErrorLocation)
            ]
        ]


viewGraphqlErrorLocation : Graphql.Http.GraphqlError.Location -> Html msg
viewGraphqlErrorLocation location =
    dl []
        [ dt [] [ text "line" ]
        , dd [] [ text <| String.fromInt location.line ]
        , dt [] [ text "column" ]
        , dd [] [ text <| String.fromInt location.column ]
        ]


viewMetadata : Http.Metadata -> Html msg
viewMetadata metadata =
    dl []
        [ dt [] [ text "url" ]
        , dd [] [ text metadata.url ]
        , dt [] [ text "statusCode" ]
        , dd [] [ text <| String.fromInt metadata.statusCode ]
        , dt [] [ text "statusText" ]
        , dd [] [ text metadata.statusText ]
        , dt [] [ text "headers" ]
        , dd [] (viewHeaders metadata.headers)
        ]


viewHeaders : Dict.Dict String String -> List (Html msg)
viewHeaders headers =
    Dict.foldl addHeader [] headers


addHeader : String -> String -> List (Html msg) -> List (Html msg)
addHeader key value headers =
    headers ++ [ dt [] [ text key ], dd [] [ text value ] ]


badUrl url =
    text <| "Bad url: " ++ url


timeout =
    text "Timeout"


networkError =
    text "Network error"


someStatus metadata body badOrGood =
    div [] [ text <| badOrGood ++ "Status: " ++ body, viewMetadata metadata ]


badStatus metadata body =
    let
        errorDecoder =
            Json.Decode.map ApiError
                (Json.Decode.field "error" Json.Decode.string)
    in
    case Json.Decode.decodeString errorDecoder body of
        Ok e ->
            dl []
                [ dt [] [ text "API error" ]
                , dd [] [ text e.error ]
                ]

        Err _ ->
            "Bad" |> someStatus metadata body


goodStatus metadata body =
    "Good" |> someStatus metadata body


badPayload error =
    div [] [ text "HttpError: BadPayload", pre [] [ text <| Json.Decode.errorToString error ] ]


type alias ApiError =
    { error : String
    }
