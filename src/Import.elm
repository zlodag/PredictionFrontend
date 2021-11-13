module Import exposing (Data, DecryptionMessage, fetchGroups, initialData, onSubmitCasesResult, queryRequest, toDecryptionCmd, updateWithDecrypted, view)

import Common exposing (HtmlRemoteData, NamedNodeData, Now, displayOutcome, displayTime)
import Config exposing (api, privateGroupName)
import Dict
import Error
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html, button, dd, dl, dt, form, input, label, li, option, select, text, ul)
import Html.Attributes exposing (checked, disabled, for, id, selected, step, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as D
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject
import Predictions.Mutation
import Predictions.Object.Group
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import ScalarCodecs exposing (Timestamp)
import Url.Builder


type Data
    = Data GroupParams ImportParams (RemoteData Error ImportedData)


type alias ImportedData =
    { userId : Int
    , count : Int
    , cases : CaseDict
    , userName : String
    , userEmail : String
    }


type alias ImportParams =
    { apiToken : String
    , pageSize : Int
    , page : Int
    , password : String
    , lastImport : HtmlRemoteData Int
    }


initialParams : ImportParams
initialParams =
    { apiToken = ""
    , pageSize = 10
    , page = 1
    , password = ""
    , lastImport = NotAsked
    }


initialData : List NamedNodeData -> Data
initialData myGroups =
    Data (GroupParams Nothing myGroups) initialParams NotAsked


type alias CaseDict =
    Dict.Dict Int ImportedCase


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | ApiError String String
    | DecodeError D.Error


type alias ImportedCase =
    { remoteId : Int
    , encryptedReference : String
    , decryptedReference : Maybe String
    , created : Timestamp
    , deadline : Timestamp
    , diagnoses : List ImportedPrediction
    , comments : List ImportedComment
    , selected : Bool
    }


type alias DecryptionMessage =
    { password : String
    , ciphers : List String
    }


toDecryptionCmd : (DecryptionMessage -> Cmd msg) -> Data -> Cmd msg
toDecryptionCmd toCmd (Data _ params remoteData) =
    case remoteData of
        Success data ->
            List.map .encryptedReference (Dict.values data.cases) |> DecryptionMessage params.password |> toCmd

        _ ->
            Cmd.none


updateWithDecrypted : D.Value -> Data -> Data
updateWithDecrypted decodedReferences data =
    case ( data, decodedReferences |> D.decodeValue (D.dict D.string) ) of
        ( Data groupParams importParams (Success importedData), Ok references ) ->
            let
                mapFn : Int -> ImportedCase -> ImportedCase
                mapFn _ importedCase =
                    { importedCase | decryptedReference = references |> Dict.get importedCase.encryptedReference }
            in
            { importedData | cases = importedData.cases |> Dict.map mapFn }
                |> Success
                |> Data groupParams importParams

        _ ->
            data


type alias ImportedPrediction =
    { diagnosis : String
    , confidence : Int
    , outcome : Maybe Outcome
    }


type alias ImportedComment =
    { timestamp : Timestamp
    , text : String
    }


type alias GroupParams =
    { selected : Maybe Id
    , myGroups : List NamedNodeData
    }


fetchGroups : Id -> Graphql.Http.Request (List NamedNodeData)
fetchGroups userId =
    SelectionSet.map2 NamedNodeData Predictions.Object.Group.id Predictions.Object.Group.name
        |> Predictions.Object.User.groups
        |> Predictions.Query.user { id = userId }
        |> Graphql.Http.queryRequest api


queryRequest : (Data -> msg) -> Data -> Cmd msg
queryRequest resultToMsg (Data groupParams importParams _) =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://predictionbook.com"
                [ "api", "my_predictions" ]
                [ Url.Builder.string "api_token" importParams.apiToken
                , Url.Builder.int "page_size" importParams.pageSize
                , Url.Builder.int "page" importParams.page
                ]
        , expect = Http.expectStringResponse (RemoteData.fromResult >> Data groupParams importParams >> resultToMsg) toResult
        }


toResult : Http.Response String -> Result Error ImportedData
toResult response =
    case response of
        Http.GoodStatus_ _ body ->
            case D.decodeString dataDecoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err <| DecodeError err

        Http.BadUrl_ url ->
            Err <| BadUrl url

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <|
                case D.decodeString (D.map2 ImportError (D.field "error" D.string) (D.field "status" D.string)) body of
                    Ok e ->
                        ApiError e.error e.status

                    Err _ ->
                        BadStatus metadata body


type alias RemotePrediction =
    { created : Timestamp
    , predictionGroup : Maybe ImportedPredictionGroupData
    , diagnosis : String
    , meanConfidence : Int
    , deadline : Timestamp
    , outcome : Maybe Outcome
    , responses : List ImportedResponse
    }


type alias ImportedResponse =
    { timestamp : Timestamp
    , comment : Maybe String
    , confidence : Maybe Int
    , userId : Int
    }


type alias ImportedPredictionGroupData =
    { groupId : Int
    , descriptionWithGroup : String
    }


dataDecoder : D.Decoder ImportedData
dataDecoder =
    let
        decoder : Int -> D.Decoder ImportedData
        decoder userId =
            let
                mapToData : List RemotePrediction -> String -> String -> ImportedData
                mapToData predictions =
                    ImportedData
                        userId
                        (List.length predictions)
                        (createImportedCases predictions)
            in
            D.map3 mapToData
                (predictionDecoder userId |> D.list |> D.field "predictions")
                (D.at [ "user", "name" ] D.string)
                (D.at [ "user", "email" ] D.string)
    in
    D.at [ "user", "user_id" ] D.int |> D.andThen decoder


type alias EncryptedReference =
    { groupId : Int
    , ciphertext : String
    , cleartext : Maybe String
    }


createImportedCases : List RemotePrediction -> CaseDict
createImportedCases importedPredictions =
    let
        predictionToDiagnosis : RemotePrediction -> ImportedPrediction
        predictionToDiagnosis prediction =
            let
                foldConfidence : ImportedResponse -> Int -> Int
                foldConfidence response confidence =
                    case response.confidence of
                        Just c ->
                            c

                        Nothing ->
                            confidence
            in
            ImportedPrediction prediction.diagnosis (List.foldr foldConfidence prediction.meanConfidence prediction.responses) prediction.outcome

        predictionToComments : RemotePrediction -> List ImportedComment
        predictionToComments prediction =
            let
                filter : ImportedResponse -> Maybe ImportedComment
                filter response =
                    Maybe.map (ImportedComment response.timestamp) response.comment
            in
            List.filterMap filter prediction.responses

        upsert : RemotePrediction -> CaseDict -> CaseDict
        upsert prediction dict =
            case prediction.predictionGroup of
                Nothing ->
                    dict

                Just group ->
                    let
                        case_ =
                            Dict.get group.groupId dict
                                |> Maybe.withDefault
                                    (ImportedCase
                                        group.groupId
                                        (String.slice 1 -(2 + String.length prediction.diagnosis) group.descriptionWithGroup)
                                        Nothing
                                        prediction.created
                                        prediction.deadline
                                        []
                                        []
                                        True
                                    )
                    in
                    Dict.insert group.groupId
                        { case_
                            | diagnoses = predictionToDiagnosis prediction :: case_.diagnoses
                            , comments = predictionToComments prediction ++ case_.comments
                        }
                        dict
    in
    List.foldl upsert Dict.empty importedPredictions


predictionDecoder : Int -> D.Decoder RemotePrediction
predictionDecoder userId =
    let
        mapBoolToOutcome : Bool -> Outcome
        mapBoolToOutcome bool =
            if bool then
                Right

            else
                Wrong
    in
    D.map7 RemotePrediction
        (D.field "created_at" Iso8601.decoder)
        (D.maybe <| D.map2 ImportedPredictionGroupData (D.field "prediction_group_id" D.int) (D.field "description_with_group" D.string))
        (D.field "description" D.string)
        (D.field "mean_confidence" D.int)
        (D.field "deadline" Iso8601.decoder)
        (D.field "outcome" <| D.nullable (D.map mapBoolToOutcome D.bool))
        (D.field "responses" <| responseListDecoder userId)


responseListDecoder : Int -> D.Decoder (List ImportedResponse)
responseListDecoder userId =
    let
        foldFn : ImportedResponse -> List ImportedResponse -> List ImportedResponse
        foldFn response list =
            if response.userId == userId then
                response :: list

            else
                list
    in
    D.map4 ImportedResponse
        (D.field "created_at" Iso8601.decoder)
        (D.field "comment" <| D.nullable D.string)
        (D.field "confidence" <| D.nullable D.int)
        (D.field "user_id" D.int)
        |> D.list
        |> D.map (List.foldl foldFn [])


type alias ImportError =
    { error : String
    , status : String
    }


view : (Data -> msg) -> (Data -> msg) -> (Data -> Graphql.Http.Request Int -> msg) -> Now -> Data -> Html msg
view sendRequestMsg updateModelMsg submitCasesMsg now (Data groupParams importParams remoteData) =
    let
        getCases : ImportParams -> msg
        getCases newParams =
            sendRequestMsg <| Data groupParams newParams Loading

        changeImportParams : ImportParams -> msg
        changeImportParams newParams =
            updateModelMsg <| Data groupParams newParams remoteData
    in
    case remoteData of
        Success importedData ->
            displayImportedData getCases updateModelMsg submitCasesMsg now importParams importedData groupParams

        NotAsked ->
            let
                changeApiToken apiToken =
                    changeImportParams { importParams | apiToken = apiToken }

                changePageSize pageSize =
                    changeImportParams { importParams | pageSize = Maybe.withDefault 1000 <| String.toInt pageSize }

                changePage page =
                    changeImportParams { importParams | page = Maybe.withDefault 1 <| String.toInt page }

                changePassword password =
                    changeImportParams { importParams | password = password }
            in
            form [ onSubmit <| getCases importParams ]
                [ label [ for "api_token" ] [ text "API token: " ]
                , input [ id "api_token", value importParams.apiToken, onInput changeApiToken ] []
                , label [ for "password" ] [ text "Decryption password: " ]
                , input [ id "password", type_ "password", value importParams.password, onInput changePassword ] []
                , label [ for "page_size" ] [ text "Page size: " ]
                , input [ id "page_size", type_ "number", Html.Attributes.min "1", Html.Attributes.max "1000", step "1", value <| String.fromInt importParams.pageSize, onInput changePageSize ] []
                , label [ for "page" ] [ text "Page: " ]
                , input [ id "page", type_ "number", Html.Attributes.min "1", step "1", value <| String.fromInt importParams.page, onInput changePage ] []
                , button [ type_ "submit" ] [ text "Import!" ]
                ]

        Loading ->
            text "Loading..."

        Failure error ->
            case error of
                BadUrl string ->
                    Error.httpErrorToHtml <| Graphql.Http.BadUrl string

                Timeout ->
                    Error.httpErrorToHtml <| Graphql.Http.Timeout

                NetworkError ->
                    Error.httpErrorToHtml <| Graphql.Http.NetworkError

                BadStatus metadata string ->
                    Error.httpErrorToHtml <| Graphql.Http.BadStatus metadata string

                ApiError apiError status ->
                    dl []
                        [ dt [] [ text "API error" ]
                        , dd [] [ text apiError ]
                        , dt [] [ text "Status" ]
                        , dd [] [ text status ]
                        ]

                DecodeError decodeError ->
                    dl []
                        [ dt [] [ text "Decode error" ]
                        , dd [] [ text <| D.errorToString decodeError ]
                        ]


displayImportedData : (ImportParams -> msg) -> (Data -> msg) -> (Data -> Graphql.Http.Request Int -> msg) -> Now -> ImportParams -> ImportedData -> GroupParams -> Html msg
displayImportedData getCases updateModelMsg submitCasesMsg now importParams importedData groupParams =
    let
        selectedCases : List ImportedCase
        selectedCases =
            let
                acc : a -> ImportedCase -> List ImportedCase -> List ImportedCase
                acc _ case_ list =
                    if case_.selected then
                        case_ :: list

                    else
                        list
            in
            Dict.foldr acc [] importedData.cases

        private =
            "_private"

        onChangeGroup : String -> msg
        onChangeGroup value =
            updateModelMsg <|
                Data
                    { groupParams
                        | selected =
                            if value == private then
                                Nothing

                            else
                                Just <| Id value
                    }
                    importParams
                    (Success importedData)

        mapToOption : NamedNodeData -> Html msg
        mapToOption group =
            case group.id of
                Id groupId ->
                    option
                        [ value groupId
                        , selected
                            (case groupParams.selected of
                                Just (Id id) ->
                                    id == groupId

                                Nothing ->
                                    False
                            )
                        ]
                        [ text group.name ]

        toggleSelected : Int -> Bool -> msg
        toggleSelected caseId selected =
            let
                updateFn : ImportedCase -> ImportedCase
                updateFn case_ =
                    { case_ | selected = selected }
            in
            { importedData | cases = Dict.update caseId (Maybe.map updateFn) importedData.cases }
                |> Success
                |> Data groupParams importParams
                |> updateModelMsg

        mapToCaseInput : ImportedCase -> Predictions.InputObject.ImportedCaseInput
        mapToCaseInput case_ =
            Predictions.InputObject.ImportedCaseInput
                (Maybe.withDefault case_.encryptedReference case_.decryptedReference)
                case_.created
                (OptionalArgument.fromMaybe groupParams.selected)
                case_.deadline
                (case_.diagnoses |> List.map mapToPredictionInput)
                (case_.comments |> List.map mapToCommentInput)

        mapToPredictionInput : ImportedPrediction -> Predictions.InputObject.ImportedPredictionInput
        mapToPredictionInput prediction =
            Predictions.InputObject.ImportedPredictionInput
                prediction.diagnosis
                prediction.confidence
                (OptionalArgument.fromMaybe prediction.outcome)

        mapToCommentInput : ImportedComment -> Predictions.InputObject.ImportedCommentInput
        mapToCommentInput comment =
            Predictions.InputObject.ImportedCommentInput
                comment.text
                comment.timestamp

        submitCasesRequest : List Predictions.InputObject.ImportedCaseInput -> Graphql.Http.Request Int
        submitCasesRequest cases =
            Predictions.Mutation.importCases { cases = cases }
                |> Graphql.Http.mutationRequest api
    in
    dl []
        [ dt [] [ text "API token" ]
        , dd [] [ text importParams.apiToken ]
        , dt [] [ text "Page" ]
        , dd [] <|
            (if importParams.page > 1 then
                [ button [ type_ "button", onClick <| getCases <| { importParams | page = importParams.page - 1 } ] [ text "<" ] ]

             else
                []
            )
                ++ [ text <| String.fromInt importParams.page ]
                ++ (if importParams.pageSize == importedData.count then
                        [ button [ type_ "button", onClick <| getCases <| { importParams | page = importParams.page + 1 } ] [ text ">" ] ]

                    else
                        []
                   )
        , dt [] [ text "name" ]
        , dd [] [ text importedData.userName ]
        , dt [] [ text "email" ]
        , dd [] [ text importedData.userEmail ]
        , dt [] [ text "user_id" ]
        , dd [] [ text <| String.fromInt importedData.userId ]
        , dt [] [ text "Import to group" ]
        , dd [] [ select [ onInput onChangeGroup ] <| option [ value private, selected (groupParams.selected == Nothing) ] [ text privateGroupName ] :: List.map mapToOption groupParams.myGroups ]
        , dt [] [ text "Last import" ]
        , dd []
            [ case importParams.lastImport of
                NotAsked ->
                    text "N/A"

                Loading ->
                    text "Loading"

                Failure error ->
                    Error.graphqlHttpErrorToHtml error

                Success n ->
                    text <| "Added " ++ String.fromInt n ++ " cases"
            ]
        , dt []
            [ text <| "Predictions (" ++ (String.fromInt <| List.length selectedCases) ++ " selected)"
            , button [ type_ "button", disabled <| List.isEmpty selectedCases, selectedCases |> List.map mapToCaseInput |> submitCasesRequest |> submitCasesMsg (Data groupParams { importParams | lastImport = Loading } <| Success importedData) |> onClick ] [ text "Import" ]
            ]
        , dd []
            [ ul [] <| List.map (\( caseId, case_ ) -> displayImportedCase now (toggleSelected caseId) case_ |> List.singleton >> li []) <| Dict.toList importedData.cases
            ]
        ]


displayImportedCase : Now -> (Bool -> msg) -> ImportedCase -> Html msg
displayImportedCase now toggleSelected case_ =
    li []
        [ dl []
            [ dt [] [ text "Import" ]
            , dd [] [ input [ type_ "checkbox", checked case_.selected, onCheck toggleSelected ] [] ]
            , dt [] [ text "Reference" ]
            , dd [] [ text <| Maybe.withDefault case_.encryptedReference case_.decryptedReference ]
            , dt [] [ text "Created" ]
            , dd [] [ displayTime now case_.created ]
            , dt [] [ text "Deadline" ]
            , dd [] [ displayTime now case_.deadline ]
            , dt [] [ text "Diagnoses" ]
            , dd [] [ ul [] <| List.map (displayImportedDiagnosis >> List.singleton >> li []) case_.diagnoses ]
            , dt [] [ text "Comments" ]
            , dd [] [ ul [] <| List.map (displayImportedComment now >> List.singleton >> li []) case_.comments ]
            ]
        ]


displayImportedDiagnosis : ImportedPrediction -> Html msg
displayImportedDiagnosis importedDiagnosis =
    dl []
        [ dt [] [ text "Diagnosis" ]
        , dd [] [ text importedDiagnosis.diagnosis ]
        , dt [] [ text "Confidence" ]
        , dd [] [ text <| String.fromInt importedDiagnosis.confidence ++ "%" ]
        , dt [] [ text "Outcome" ]
        , dd [] [ text <| Maybe.withDefault "Pending" <| Maybe.map displayOutcome importedDiagnosis.outcome ]
        ]


displayImportedComment : Now -> ImportedComment -> Html msg
displayImportedComment now importedComment =
    dl []
        [ dt [] [ displayTime now importedComment.timestamp ]
        , dd [] [ text importedComment.text ]
        ]


onSubmitCasesResult : Data -> HtmlRemoteData Int -> Data
onSubmitCasesResult (Data groupParams importParams remoteData) lastImport =
    Data groupParams { importParams | lastImport = lastImport } remoteData
