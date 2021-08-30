module Helper exposing (GraphqlRemoteData, NamedNodeData, PredictionData, UserCandidate, blankPrediction, displayGroupSelect, validateConfidence, validateDeadline, viewData)

import Dict exposing (Dict)
import FormField
import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError exposing (GraphqlError, Location, PossiblyParsedData(..))
import Html exposing (Html, dd, div, dl, dt, li, option, pre, select, text, ul)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Http
import Iso8601
import Json.Decode as Decode
import List exposing (map)
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData)
import ScalarCodecs exposing (Timestamp)
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


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserCandidate =
    { node : NamedNodeData
    , groups : List NamedNodeData
    }


type alias PredictionData =
    { diagnosis : FormField.Field String
    , confidence : FormField.Field Int
    }


blankPrediction =
    PredictionData (FormField.newNonEmptyStringField "Diagnosis") (FormField.newField validateConfidence)


displayGroupSelect : Maybe Id -> List NamedNodeData -> (Maybe Id -> msg) -> Html msg
displayGroupSelect currentGroupId groups onGroupChanged =
    let
        determineSelected : Id -> Bool
        determineSelected id =
            currentGroupId == Just id

        mapToOption : NamedNodeData -> Html msg
        mapToOption node =
            case node.id of
                Id id ->
                    option [ value id, selected <| determineSelected node.id ] [ text node.name ]

        getNewGroupId : Id -> Maybe Id
        getNewGroupId newGroupId =
            case List.filter (\group -> group.id == newGroupId) groups of
                [ newGroup ] ->
                    Just newGroup.id

                _ ->
                    Nothing

        onSelectionChanged : String -> msg
        onSelectionChanged inputValue =
            onGroupChanged <|
                if String.isEmpty inputValue then
                    Nothing

                else
                    Id inputValue |> getNewGroupId
    in
    select [ onInput onSelectionChanged ] <|
        option [ value "", selected (currentGroupId == Nothing) ] [ text "None" ]
            :: List.map mapToOption groups


validateDeadline : String -> Result String Timestamp
validateDeadline deadline =
    let
        errorMapper _ =
            "Could not parse as ISO-8601 datetime string"
    in
    Result.mapError errorMapper <| Iso8601.toTime deadline


validateConfidence : String -> Result String Int
validateConfidence confidence =
    case String.toInt confidence of
        Just a ->
            if a < 0 then
                Err "Minimum confidence is 0%"

            else if a > 100 then
                Err "Maximum confidence is 100%"

            else
                Ok a

        Nothing ->
            Err "Enter an integer from 0 to 100"
