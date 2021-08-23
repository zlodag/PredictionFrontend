module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Graphql.Http exposing (Error, HttpError(..), Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, viewData)
import Html exposing (Html, a, b, blockquote, button, dd, div, dl, dt, fieldset, form, h4, hr, input, label, li, option, select, span, strong, text, ul)
import Html.Attributes exposing (disabled, href, placeholder, required, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601 exposing (fromTime)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject exposing (CaseInput, PredictionInput)
import Predictions.Mutation as Mutation
import Predictions.Object.Case as Case
import Predictions.Object.Comment as Comment
import Predictions.Object.Diagnosis as Diagnosis
import Predictions.Object.Group as Group
import Predictions.Object.Judgement as Judgement
import Predictions.Object.User as User
import Predictions.Object.Wager as Wager
import Predictions.Query as Query exposing (GroupsOptionalArguments)
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..))
import ScalarCodecs exposing (Id, Timestamp)
import String
import Time
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias PredictionData =
    { diagnosis : FormField String
    , confidence : FormField Int
    }


type alias InputValue =
    String


type alias ValidationMessage =
    String


type FormField a
    = Valid a InputValue
    | Invalid ValidationMessage InputValue


type alias CaseData =
    { groupId : Maybe Id
    , reference : FormField String
    , predictions : List PredictionData
    , deadline : FormField Timestamp
    }


type State
    = WelcomePage
    | UserDetail (GraphqlRemoteData UserDetailResponse)
    | GroupList (GraphqlRemoteData GroupListResponse)
    | GroupDetail (GraphqlRemoteData GroupDetailResponse)
    | CaseDetail (GraphqlRemoteData CaseDetailResponse)
    | NewCase CaseData
    | NoData


type alias Model =
    { key : Nav.Key
    , state : State
    , auth : Auth
    }


type Auth
    = SignedIn (List UserCandidate) UserCandidate
    | SignedOut (List UserCandidate)


usersResponseHandler : Url.Url -> GraphqlRemoteData UserListResponse -> Msg
usersResponseHandler url response =
    GotAuth url <|
        case response of
            Success users ->
                case List.head users of
                    Just a ->
                        SignedIn users a

                    Nothing ->
                        SignedOut users

            _ ->
                SignedOut []


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key NoData <| SignedOut []
    , userListQuery |> makeRequest (usersResponseHandler url)
    )



--parseUrlAndRequest (Model key NoData Nothing) url
-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotAuth Url.Url Auth
    | UserChanged Auth
    | GotResponse State
    | ChangeNewCase CaseData
    | SubmitCase CaseInput
    | SubmitComment CaseDetailData Id String
    | SubmitNewGroup CaseDetailData (Maybe Id)
    | SubmitNewWager CaseDetailData Id Int
    | CaseCreated (GraphqlRemoteData Id)



--| CommentCreated (GraphqlRemoteData CommentData)
--| GotUserGroups Id (GraphqlRemoteData GroupListResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/graphql" then
                        ( model, Nav.load (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            parseUrlAndRequest model url

        GotAuth url auth ->
            parseUrlAndRequest { model | auth = auth } url

        UserChanged auth ->
            ( { model | auth = auth, state = NoData }
            , Nav.pushUrl model.key "/"
            )

        GotResponse graphqlRemoteData ->
            ( { model | state = graphqlRemoteData }
            , Cmd.none
            )

        ChangeNewCase caseData ->
            ( { model | state = NewCase caseData }
            , Cmd.none
            )

        CaseCreated graphqlRemoteData ->
            case graphqlRemoteData of
                Success (Id id) ->
                    ( model, Nav.pushUrl model.key <| "/case/" ++ id )

                _ ->
                    ( model, Cmd.none )

        SubmitCase caseData ->
            case model.auth of
                SignedIn _ currentUser ->
                    ( model, submitCase { caseData | creatorId = currentUser.id } )

                SignedOut _ ->
                    ( model, Cmd.none )

        SubmitComment caseDetail creatorId string ->
            let
                alterCase : GraphqlRemoteData CommentData -> Msg
                alterCase data =
                    GotResponse
                        << CaseDetail
                        << Success
                    <|
                        Just
                            { caseDetail
                                | newComment = Nothing
                                , comments =
                                    caseDetail.comments
                                        ++ (case data of
                                                Success a ->
                                                    [ a ]

                                                _ ->
                                                    []
                                           )
                            }
            in
            ( model
            , mapToCommentData
                |> Mutation.addComment (Mutation.AddCommentRequiredArguments creatorId caseDetail.node.id string)
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> alterCase)
            )

        SubmitNewGroup caseDetail groupId ->
            let
                newGroupId =
                    case groupId of
                        Just id ->
                            Present id

                        Nothing ->
                            Null

                alterGroup : GraphqlRemoteData (Maybe NamedNodeData) -> Msg
                alterGroup newGroupResult =
                    GotResponse
                        << CaseDetail
                        << Success
                        << Just
                    <|
                        case newGroupResult of
                            Success newGroup ->
                                { caseDetail
                                    | ownerGroup =
                                        case caseDetail.ownerGroup of
                                            NotOwner owner _ ->
                                                NotOwner owner newGroup

                                            OwnerViewing owner _ userGroups ->
                                                OwnerViewing owner newGroup userGroups

                                            OwnerEditing owner _ userGroups ->
                                                OwnerViewing owner newGroup userGroups
                                }

                            _ ->
                                caseDetail
            in
            ( model
            , Mutation.changeGroup
                (\args -> { args | newGroupId = newGroupId })
                (Mutation.ChangeGroupRequiredArguments caseDetail.node.id)
                (SelectionSet.map2 NamedNodeData Group.id Group.name)
                |> Graphql.Http.mutationRequest graphQlEndpoint
                |> Graphql.Http.send (RemoteData.fromResult >> alterGroup)
            )

        SubmitNewWager caseDetail diagnosisId confidence ->
            let
                filterDiagnoses : WagerData -> DiagnosisDetailData -> DiagnosisDetailData
                filterDiagnoses newWager old =
                    if old.node.id == diagnosisId then
                        { old
                            | wagers = old.wagers ++ [ newWager ]
                            , judgement =
                                case old.judgement of
                                    NotJudged (Just _) ->
                                        NotJudged Nothing

                                    _ ->
                                        old.judgement
                        }

                    else
                        old

                alterCase : GraphqlRemoteData WagerData -> Msg
                alterCase newWagerResult =
                    GotResponse
                        << CaseDetail
                        << Success
                        << Just
                    <|
                        case newWagerResult of
                            Success newWager ->
                                { caseDetail | diagnoses = List.map (filterDiagnoses newWager) caseDetail.diagnoses }

                            _ ->
                                caseDetail
            in
            ( model
            , case model.auth of
                SignedIn _ user ->
                    Mutation.addWager
                        (Mutation.AddWagerRequiredArguments user.id diagnosisId confidence)
                        mapToWagerData
                        |> Graphql.Http.mutationRequest graphQlEndpoint
                        |> Graphql.Http.send (RemoteData.fromResult >> alterCase)

                _ ->
                    Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Predictions"
    , body =
        [ displayAuth model.auth
        , ul [] <|
            [ li [] [ a [ href "/" ] [ text "Home" ] ]
            , li [] [ a [ href "/graphql" ] [ text "GraphiQL" ] ]
            ]
                ++ (case model.auth of
                        SignedIn _ userCandidate ->
                            case userCandidate.id of
                                Id userId ->
                                    [ li [] [ a [ href <| "/user/" ++ userId ] [ text "My user page" ] ]
                                    , li [] [ a [ href "/new" ] [ text "New prediction" ] ]
                                    ]

                        SignedOut _ ->
                            []
                   )
        , hr [] []
        , displayData model
        ]
    }


determineNewAuth : Auth -> String -> Auth
determineNewAuth auth id =
    let
        users =
            case auth of
                SignedIn a _ ->
                    a

                SignedOut a ->
                    a
    in
    case List.filter (\user -> user.id == Id id) users of
        [ newUser ] ->
            SignedIn users newUser

        _ ->
            SignedOut users


displayAuth : Auth -> Html Msg
displayAuth auth =
    div []
        [ label [] [ text "Select current user: " ]
        , select [ onInput <| determineNewAuth auth >> UserChanged ] <|
            List.map
                (determineSelected
                    (case auth of
                        SignedIn _ currentUser ->
                            Just currentUser.id

                        SignedOut _ ->
                            Nothing
                    )
                    |> displayOption
                )
            <|
                NamedNodeData (Id "") "<No user>"
                    :: List.map (\user -> NamedNodeData user.id user.name)
                        (case auth of
                            SignedIn users _ ->
                                users

                            SignedOut users ->
                                users
                        )
        ]


displayMaybe : (a -> Html msg) -> Maybe a -> Html msg
displayMaybe displayFunction data =
    case data of
        Just a ->
            displayFunction a

        Nothing ->
            div [] [ text "Not found" ]


displayData : Model -> Html Msg
displayData model =
    case model.state of
        WelcomePage ->
            text <| welcomeMessage model.auth

        NoData ->
            text "No data!"

        UserDetail graphqlRemoteData ->
            viewData (displayMaybe displayUser) graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group") graphqlRemoteData

        GroupDetail graphqlRemoteData ->
            viewData (displayMaybe displayGroup) graphqlRemoteData

        CaseDetail graphqlRemoteData ->
            viewData (displayMaybe <| displayCase model.auth) graphqlRemoteData

        NewCase caseData ->
            case model.auth of
                SignedIn _ currentUser ->
                    displayNewForm currentUser.groups caseData

                _ ->
                    text "Sign in first"


welcomeMessage : Auth -> String
welcomeMessage auth =
    case auth of
        SignedIn _ userCandidate ->
            "Welcome, " ++ userCandidate.name

        SignedOut _ ->
            "Please sign in to begin"


displayNamedNodeLink : String -> NamedNodeData -> Html msg
displayNamedNodeLink base_path node =
    case node.id of
        Id id ->
            a [ href <| base_path ++ "/" ++ id ] [ text node.name ]


displayNamedNodeList : String -> List NamedNodeData -> Html msg
displayNamedNodeList base_path nodes =
    ul [] <| displayListItems (displayNamedNodeLink base_path >> List.singleton) nodes


displayListItems : (a -> List (Html msg)) -> List a -> List (Html msg)
displayListItems displayFunction list =
    List.map (li [] << displayFunction) list


displayUser : UserDetailData -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/user" user.node ]
        , dt [] [ text "Created" ]
        , dd [] [ text <| fromTime user.created ]
        , dt [] [ text <| "Groups (" ++ String.fromInt (List.length user.groups) ++ ")" ]
        , dd [] [ displayNamedNodeList "/group" user.groups ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length user.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case" user.cases ]
        ]


displayGroup : GroupDetailData -> Html msg
displayGroup group =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ displayNamedNodeLink "/group" group.node ]
        , dt [] [ text <| "Members (" ++ String.fromInt (List.length group.members) ++ ")" ]
        , dd [] [ displayNamedNodeList "/user" group.members ]
        , dt [] [ text <| "Cases (" ++ String.fromInt (List.length group.cases) ++ ")" ]
        , dd [] [ displayNamedNodeList "/case" group.cases ]
        ]


displayCaseGroup : CaseDetailData -> List (Html Msg)
displayCaseGroup caseDetail =
    let
        viewGroup group =
            case group of
                Just g ->
                    displayNamedNodeLink "/group" g

                Nothing ->
                    text "None"
    in
    case caseDetail.ownerGroup of
        NotOwner _ g ->
            [ viewGroup g ]

        OwnerViewing owner group groups ->
            [ div [] [ viewGroup group ]
            , div [] [ button [ onClick <| changeCase { caseDetail | ownerGroup = OwnerEditing owner group groups } ] [ text "Change" ] ]
            ]

        OwnerEditing owner group groups ->
            [ div []
                [ SubmitNewGroup caseDetail
                    |> displayGroupSelect
                        (case group of
                            Just g ->
                                Just g.id

                            Nothing ->
                                Nothing
                        )
                        groups
                ]
            , div [] [ button [ onClick <| changeCase { caseDetail | ownerGroup = OwnerViewing owner group groups } ] [ text "Cancel" ] ]
            ]


displayCase : Auth -> CaseDetailData -> Html Msg
displayCase auth caseDetail =
    dl []
        [ dt [] [ text "Reference" ]
        , dd [] [ displayNamedNodeLink "/case" caseDetail.node ]
        , dt [] [ text "Deadline" ]
        , dd [] [ text <| fromTime caseDetail.deadline ]
        , dt [] [ text "Creator" ]
        , dd []
            [ displayNamedNodeLink "/user"
                (case caseDetail.ownerGroup of
                    NotOwner a _ ->
                        a

                    OwnerViewing a _ _ ->
                        a

                    OwnerEditing a _ _ ->
                        a
                )
            ]
        , dt [] [ text "Group" ]
        , dd [] <| displayCaseGroup caseDetail
        , dt [] [ text <| "Diagnoses (" ++ String.fromInt (List.length caseDetail.diagnoses) ++ ")" ]
        , dd [] [ ul [] <| displayListItems (displayDiagnosis auth caseDetail) caseDetail.diagnoses ]
        , dt [] [ text <| "Comments (" ++ String.fromInt (List.length caseDetail.comments) ++ ")" ]
        , dd []
            [ ul [] <|
                displayListItems displayComment caseDetail.comments
                    ++ (case auth of
                            SignedIn _ currentUser ->
                                [ li [] <| displayNewComment currentUser caseDetail ]

                            SignedOut _ ->
                                []
                       )
            ]
        ]


displayDiagnosis : Auth -> CaseDetailData -> DiagnosisDetailData -> List (Html Msg)
displayDiagnosis auth caseDetail diagnosis =
    let
        toDiagnosisWithNewWager : String -> (DiagnosisDetailData -> DiagnosisDetailData)
        toDiagnosisWithNewWager inputValue =
            toDiagnosisWithJudgement << NotJudged << Just <| validateConfidence inputValue

        toDiagnosisWithJudgement : Judgement -> DiagnosisDetailData -> DiagnosisDetailData
        toDiagnosisWithJudgement judgement old =
            case old.judgement of
                Judged _ ->
                    old

                NotJudged _ ->
                    { old
                        | judgement =
                            if old.node.id == diagnosis.node.id then
                                judgement

                            else
                                NotJudged Nothing
                    }

        toDiagnosisWithoutNewWager old =
            case old.judgement of
                Judged _ ->
                    old

                NotJudged _ ->
                    { old | judgement = NotJudged Nothing }

        changeWager : String -> Msg
        changeWager newValue =
            changeCase { caseDetail | diagnoses = List.map (toDiagnosisWithNewWager newValue) caseDetail.diagnoses }
    in
    [ h4 [] [ text diagnosis.node.name ]
    , ul [] <|
        displayListItems displayWager diagnosis.wagers
            ++ (case ( diagnosis.judgement, auth ) of
                    ( Judged judgementData, _ ) ->
                        [ text " Judged as "
                        , strong [] [ displayOutcome judgementData.outcome ]
                        , text " by "
                        , displayNamedNodeLink "/user" judgementData.judgedBy
                        , text <| " at " ++ fromTime judgementData.timestamp ++ ")"
                        ]
                            |> li []
                            |> List.singleton

                    ( NotJudged _, SignedOut _ ) ->
                        []

                    ( NotJudged wagerField, SignedIn _ user ) ->
                        if List.any (\wager -> wager.creator.id == user.id) diagnosis.wagers then
                            []

                        else
                            case wagerField of
                                Nothing ->
                                    [ button
                                        [ type_ "button", onClick <| changeCase { caseDetail | diagnoses = List.map (toDiagnosisWithNewWager "") caseDetail.diagnoses } ]
                                        [ text "Add wager" ]
                                    ]
                                        |> li []
                                        |> List.singleton

                                Just newWager ->
                                    let
                                        ( inputValue, submitButtonDisabled, formAttributes ) =
                                            case newWager of
                                                Valid intValue val ->
                                                    ( val, False, [ onSubmit <| SubmitNewWager caseDetail diagnosis.node.id intValue ] )

                                                Invalid _ val ->
                                                    ( val, True, [] )
                                    in
                                    [ form formAttributes
                                        [ displayNamedNodeLink "/user" <| NamedNodeData user.id user.name
                                        , text " estimated "
                                        , input [ type_ "number", step "1", Html.Attributes.min "0", Html.Attributes.max "100", value inputValue, onInput changeWager ] []
                                        , text "%"
                                        , div []
                                            [ button [ type_ "submit", disabled submitButtonDisabled ] [ text "Submit" ]
                                            , button [ type_ "button", onClick <| changeCase { caseDetail | diagnoses = List.map toDiagnosisWithoutNewWager caseDetail.diagnoses } ] [ text "Cancel" ]
                                            ]
                                        ]
                                    ]
                                        |> li []
                                        |> List.singleton
               )
    ]


changeCase : CaseDetailData -> Msg
changeCase newCase =
    newCase |> Just >> Success >> CaseDetail >> GotResponse


changeComment : CaseDetailData -> Maybe String -> Msg
changeComment caseDetail newComment =
    changeCase { caseDetail | newComment = newComment }


displayNewComment : UserCandidate -> CaseDetailData -> List (Html Msg)
displayNewComment currentUser caseDetail =
    case caseDetail.newComment of
        Just string ->
            [ displayNamedNodeLink "/user" <| NamedNodeData currentUser.id currentUser.name
            , blockquote []
                [ form [ onSubmit <| SubmitComment caseDetail currentUser.id string ]
                    [ input [ type_ "text", placeholder "Enter comment", onInput <| Just >> changeComment caseDetail ] [ text string ]
                    , button [ type_ "submit", disabled << String.isEmpty << String.trim <| string ] [ text "Submit" ]
                    , button [ type_ "button", onClick <| changeComment caseDetail Nothing ] [ text "Cancel" ]
                    ]
                ]
            ]

        Nothing ->
            [ button [ onClick <| changeComment caseDetail <| Just "" ] [ text "Add comment" ] ]


displayComment : CommentData -> List (Html msg)
displayComment comment =
    [ span [] [ displayNamedNodeLink "/user" comment.creator, text <| " at " ++ fromTime comment.timestamp ]
    , blockquote [] [ text comment.text ]
    ]


displayWager : WagerData -> List (Html msg)
displayWager wager =
    [ displayNamedNodeLink "/user" wager.creator
    , text " estimated "
    , b [] [ text <| String.fromInt wager.confidence ++ "%" ]
    , text <| " at " ++ fromTime wager.timestamp
    ]


displayOutcome : Outcome -> Html msg
displayOutcome outcome =
    text
        (case outcome of
            Right ->
                "Right"

            Wrong ->
                "Wrong"

            Indeterminate ->
                "Indeterminate"
        )


validateNonEmptyField : String -> InputValue -> FormField String
validateNonEmptyField fieldName inputValue =
    let
        trimmed =
            String.trim inputValue
    in
    inputValue
        |> (if String.isEmpty trimmed then
                Invalid (fieldName ++ " is required")

            else
                Valid trimmed
           )


changeNewReference : CaseData -> String -> Msg
changeNewReference caseData reference =
    ChangeNewCase { caseData | reference = validateReference reference }


validateDeadline : InputValue -> FormField Time.Posix
validateDeadline deadline =
    deadline
        |> (case Iso8601.toTime deadline of
                Ok value ->
                    Valid value

                _ ->
                    Invalid "Could not parse as ISO-8601 datetime string"
           )


changeNewDeadline : CaseData -> String -> Msg
changeNewDeadline caseData deadline =
    ChangeNewCase { caseData | deadline = validateDeadline deadline }


blankPrediction =
    PredictionData (validateDiagnosis "") (validateConfidence "")


addNewLine : CaseData -> Msg
addNewLine caseData =
    ChangeNewCase { caseData | predictions = caseData.predictions ++ [ blankPrediction ] }


changeNewPrediction : Int -> PredictionData -> CaseData -> Msg
changeNewPrediction index newPrediction caseData =
    ChangeNewCase { caseData | predictions = List.take index caseData.predictions ++ [ newPrediction ] ++ List.drop (index + 1) caseData.predictions }


validateDiagnosis =
    validateNonEmptyField "Diagnosis"


validateReference =
    validateNonEmptyField "Case reference"


changeNewDiagnosis : Int -> PredictionData -> CaseData -> String -> Msg
changeNewDiagnosis index oldPrediction caseData diagnosis =
    changeNewPrediction index { oldPrediction | diagnosis = validateDiagnosis diagnosis } caseData


validateConfidence : InputValue -> FormField Int
validateConfidence confidence =
    confidence
        |> (case String.toInt confidence of
                Just a ->
                    if a < 0 then
                        Invalid "Minimum confidence is 0%"

                    else if a > 100 then
                        Invalid "Maximum confidence is 100%"

                    else
                        Valid a

                Nothing ->
                    Invalid "Enter an integer from 0 to 100"
           )


changeNewConfidence : Int -> PredictionData -> CaseData -> String -> Msg
changeNewConfidence index oldPrediction caseData confidence =
    changeNewPrediction index { oldPrediction | confidence = validateConfidence confidence } caseData


changeNewPredictionRemove : Int -> CaseData -> Msg
changeNewPredictionRemove index caseData =
    ChangeNewCase { caseData | predictions = List.take index caseData.predictions ++ List.drop (index + 1) caseData.predictions }


displayPrediction : CaseData -> Int -> PredictionData -> Html Msg
displayPrediction caseData index prediction =
    fieldset []
        [ div []
            [ label [] [ text "Diagnosis: " ]
            , input
                [ required True
                , value <| getInputValue prediction.diagnosis
                , onInput (changeNewDiagnosis index prediction caseData)
                ]
                []
            , displayValidity prediction.diagnosis
            ]
        , div []
            [ label [] [ text "Confidence (%): " ]
            , input
                [ type_ "number"
                , step "1"
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , value <| getInputValue prediction.confidence
                , onInput (changeNewConfidence index prediction caseData)
                ]
                []
            , displayValidity prediction.confidence
            ]
        , button [ onClick (changeNewPredictionRemove index caseData), disabled <| List.length caseData.predictions == 1 ] [ text "➖" ]
        ]


printPrediction : PredictionInput -> Html Msg
printPrediction prediction =
    dl []
        [ dt [] [ text "Diagnosis" ]
        , dd [] [ text prediction.diagnosis ]
        , dt [] [ text "Confidence" ]
        , dd [] [ text <| String.fromInt prediction.confidence ++ "%" ]
        ]


getInputValue : FormField a -> InputValue
getInputValue field =
    case field of
        Valid _ value ->
            value

        Invalid _ value ->
            value


displayValidity : FormField a -> Html Msg
displayValidity field =
    case field of
        Valid _ _ ->
            span [ style "background-color" "green", style "color" "white" ] [ text "✓" ]

        Invalid validationMessage _ ->
            span [ style "background-color" "red", style "color" "white" ] [ text ("✗ " ++ validationMessage) ]


displayDebug : CaseData -> Html Msg
displayDebug data =
    case prepareCaseInput data of
        Just caseData ->
            div []
                [ dl []
                    [ dt [] [ text "Reference" ]
                    , dd [] [ text caseData.reference ]
                    , dt [] [ text "Deadline" ]
                    , dd [] [ caseData.deadline |> Iso8601.fromTime >> String.dropRight 5 |> text ]
                    , dt [] [ text "Group" ]
                    , dd []
                        [ text <|
                            case caseData.groupId of
                                Present (Id id) ->
                                    id

                                _ ->
                                    "None"
                        ]
                    , dt [] [ text "Predictions" ]
                    , dd [] [ caseData.predictions |> List.map (printPrediction >> List.singleton >> li []) |> ul [] ]
                    ]
                , button [ onClick <| SubmitCase caseData ] [ text "Submit" ]
                ]

        Nothing ->
            text "..."


displayOption : (Id -> Bool) -> NamedNodeData -> Html Msg
displayOption isSelected node =
    case node.id of
        Id id ->
            option [ value id, selected <| isSelected node.id ] [ text node.name ]


caseGroupChanged : CaseData -> Maybe Id -> Msg
caseGroupChanged caseData groupId =
    ChangeNewCase { caseData | groupId = groupId }


determineSelected : Maybe Id -> Id -> Bool
determineSelected selectedId id =
    selectedId == Just id


displayGroupSelect : Maybe Id -> GroupListResponse -> (Maybe Id -> Msg) -> Html Msg
displayGroupSelect currentGroupId groups msgFn =
    let
        getNewGroupId string =
            case List.filter (\group -> group.id == Id string) groups of
                [ newGroup ] ->
                    Just newGroup.id

                _ ->
                    Nothing
    in
    select [ onInput <| msgFn << getNewGroupId ] <|
        List.map (determineSelected currentGroupId |> displayOption) <|
            NamedNodeData (Id "") "None"
                :: groups


displayNewForm : GroupListResponse -> CaseData -> Html Msg
displayNewForm userGroups caseData =
    div [] <|
        fieldset []
            [ div [] [ label [] [ text "Case reference: " ], input [ required True, value <| getInputValue caseData.reference, onInput (changeNewReference caseData) ] [], displayValidity caseData.reference ]
            , div [] [ label [] [ text "Deadline: " ], input [ required True, type_ "datetime-local", value <| getInputValue caseData.deadline, onInput (changeNewDeadline caseData) ] [], displayValidity caseData.deadline ]
            , div [] [ label [] [ text "Group: " ], displayGroupSelect caseData.groupId userGroups <| caseGroupChanged caseData ]
            ]
            :: List.indexedMap (displayPrediction caseData) caseData.predictions
            ++ [ fieldset [] [ button [ onClick (addNewLine caseData) ] [ text "➕" ] ]
               , displayDebug caseData
               ]



-- Parse URL


type Route
    = Welcome
    | User Id
    | Groups
    | Group Id
    | Case Id
    | New


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Welcome top
        , map (User << Id) <| s "user" </> string
        , map Groups <| s "groups"
        , map (Group << Id) <| s "group" </> string
        , map (Case << Id) <| s "case" </> string
        , map New <| s "new"
        ]


parseUrlAndRequest : Model -> Url.Url -> ( Model, Cmd Msg )
parseUrlAndRequest model url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                Welcome ->
                    ( { model | state = WelcomePage }, Cmd.none )

                User id ->
                    ( { model | state = UserDetail RemoteData.Loading }
                    , id |> userDetailQuery |> makeRequest (UserDetail >> GotResponse)
                    )

                Groups ->
                    ( { model | state = GroupList RemoteData.Loading }
                    , groupListQuery identity |> makeRequest (GroupList >> GotResponse)
                    )

                Group id ->
                    ( { model | state = GroupDetail RemoteData.Loading }
                    , id |> groupDetailQuery |> makeRequest (GroupDetail >> GotResponse)
                    )

                Case id ->
                    ( { model | state = GroupDetail RemoteData.Loading }
                    , id |> caseDetailQuery model.auth |> makeRequest (CaseDetail >> GotResponse)
                    )

                New ->
                    case model.auth of
                        SignedIn _ _ ->
                            ( { model
                                | state =
                                    NewCase <|
                                        CaseData
                                            Nothing
                                            (validateReference "")
                                            [ blankPrediction ]
                                            (validateDeadline "")
                              }
                            , Cmd.none
                            )

                        SignedOut _ ->
                            ( { model | state = NoData }
                            , Cmd.none
                            )

        Nothing ->
            ( { model | state = NoData }
            , Cmd.none
            )



-- New


preparePredictionInput : PredictionData -> Maybe PredictionInput
preparePredictionInput prediction =
    case prediction.diagnosis of
        Valid diagnosis _ ->
            case prediction.confidence of
                Valid confidence _ ->
                    Just <| PredictionInput diagnosis confidence

                Invalid _ _ ->
                    Nothing

        Invalid _ _ ->
            Nothing


prepareCaseInput : CaseData -> Maybe CaseInput
prepareCaseInput caseData =
    case caseData.reference of
        Valid reference _ ->
            case caseData.deadline of
                Valid deadline _ ->
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
                                (Id "")
                                (case caseData.groupId of
                                    Just id ->
                                        Present id

                                    Nothing ->
                                        Absent
                                )
                                deadline
                                predictions

                Invalid _ _ ->
                    Nothing

        Invalid _ _ ->
            Nothing


submitCase : CaseInput -> Cmd Msg
submitCase caseData =
    caseData
        |> Mutation.AddCaseRequiredArguments
        |> Mutation.addCase
        |> Graphql.Http.mutationRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> CaseCreated)



--
--
--submitComment : Mutation.AddCommentRequiredArguments -> Cmd Msg
--submitComment comment =
--    Mutation.addComment comment
--        |> Graphql.Http.mutationRequest graphQlEndpoint
--        |> Graphql.Http.send (RemoteData.fromResult >> CommentCreated)
--


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserCandidate =
    { id : Id
    , name : String
    , groups : List NamedNodeData
    }


type alias UserDetailData =
    { node : NamedNodeData
    , created : Timestamp
    , groups : List NamedNodeData
    , cases : List CaseLimitedData
    }


mapToUserDetailData =
    SelectionSet.map4 UserDetailData
        (SelectionSet.map2 NamedNodeData User.id User.name)
        User.created
        (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        (User.cases <| SelectionSet.map2 NamedNodeData Case.id Case.reference)


type alias UserDetailResponse =
    Maybe UserDetailData


userDetailQuery : Id -> SelectionSet UserDetailResponse RootQuery
userDetailQuery id =
    Query.user { id = id } <| mapToUserDetailData


type alias UserListResponse =
    List UserCandidate


userListQuery : SelectionSet UserListResponse RootQuery
userListQuery =
    Query.users <|
        SelectionSet.map3 UserCandidate
            User.id
            User.name
            (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    , cases : List CaseLimitedData
    }


mapToGroupDetailData =
    SelectionSet.map3 GroupDetailData
        (SelectionSet.map2 NamedNodeData Group.id Group.name)
        (Group.members <| SelectionSet.map2 NamedNodeData User.id User.name)
        (Group.cases <| SelectionSet.map2 NamedNodeData Case.id Case.reference)


type alias GroupDetailResponse =
    Maybe GroupDetailData


groupDetailQuery : Id -> SelectionSet GroupDetailResponse RootQuery
groupDetailQuery id =
    Query.group { id = id } <| mapToGroupDetailData


type alias GroupListResponse =
    List NamedNodeData


groupListQuery : (GroupsOptionalArguments -> GroupsOptionalArguments) -> SelectionSet GroupListResponse RootQuery
groupListQuery fillInOptionals =
    Query.groups fillInOptionals <| SelectionSet.map2 NamedNodeData Group.id Group.name


type alias CaseLimitedData =
    NamedNodeData


type alias CommentData =
    { creator : NamedNodeData
    , timestamp : Timestamp
    , text : String
    }


mapToCommentData =
    SelectionSet.map3 CommentData
        (Comment.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        Comment.timestamp
        Comment.text


type CaseOwnerGroup
    = NotOwner NamedNodeData (Maybe NamedNodeData)
    | OwnerViewing NamedNodeData (Maybe NamedNodeData) (List NamedNodeData)
    | OwnerEditing NamedNodeData (Maybe NamedNodeData) (List NamedNodeData)


type alias CaseDetailData =
    { newComment : Maybe String
    , node : NamedNodeData
    , ownerGroup : CaseOwnerGroup
    , deadline : Timestamp
    , diagnoses : List DiagnosisDetailData
    , comments : List CommentData
    }


mapToCaseDetailData auth =
    let
        ownerGroupResolver : NamedNodeData -> Maybe NamedNodeData -> CaseOwnerGroup
        ownerGroupResolver owner group =
            case auth of
                SignedOut _ ->
                    NotOwner owner group

                SignedIn _ user ->
                    if user.id /= owner.id then
                        NotOwner owner group

                    else
                        OwnerViewing owner group user.groups
    in
    SelectionSet.map5 (CaseDetailData Nothing)
        (SelectionSet.map2 NamedNodeData Case.id Case.reference)
        (SelectionSet.map2 ownerGroupResolver
            (Case.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
            (Case.group <| SelectionSet.map2 NamedNodeData Group.id Group.name)
        )
        Case.deadline
        (Case.diagnoses <| mapToDiagnosisDetailData)
        (Case.comments <| mapToCommentData)


type alias CaseDetailResponse =
    Maybe CaseDetailData


caseDetailQuery : Auth -> Id -> SelectionSet CaseDetailResponse RootQuery
caseDetailQuery auth id =
    Query.case_ { id = id } <| mapToCaseDetailData auth


type alias DiagnosisLimitedData =
    NamedNodeData


type alias DiagnosisDetailData =
    { node : NamedNodeData
    , wagers : List WagerData
    , judgement : Judgement
    }


type Judgement
    = Judged JudgementData
    | NotJudged (Maybe (FormField Int))


type alias WagerData =
    { creator : NamedNodeData
    , confidence : Int
    , timestamp : Timestamp
    }


type alias JudgementData =
    { judgedBy : NamedNodeData
    , timestamp : Timestamp
    , outcome : Outcome
    }


mapToDiagnosisDetailData =
    let
        mapFunction : Maybe JudgementData -> Judgement
        mapFunction j =
            case j of
                Just judgement ->
                    Judged judgement

                Nothing ->
                    NotJudged Nothing
    in
    SelectionSet.map3 DiagnosisDetailData
        (SelectionSet.map2 NamedNodeData Diagnosis.id Diagnosis.name)
        (Diagnosis.wagers <| mapToWagerData)
        (SelectionSet.map mapFunction <| Diagnosis.judgement <| mapToJudgementData)


mapToWagerData =
    SelectionSet.map3 WagerData
        (Wager.creator <| SelectionSet.map2 NamedNodeData User.id User.name)
        Wager.confidence
        Wager.timestamp


mapToJudgementData =
    SelectionSet.map3 JudgementData
        (Judgement.judgedBy <| SelectionSet.map2 NamedNodeData User.id User.name)
        Judgement.timestamp
        Judgement.outcome



-- Helper for fetch


makeRequest : (GraphqlRemoteData decodesTo -> Msg) -> SelectionSet decodesTo RootQuery -> Cmd Msg
makeRequest msgConstructor set =
    set
        |> Graphql.Http.queryRequest graphQlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> msgConstructor)


graphQlEndpoint =
    "http://localhost:3000/graphql"
