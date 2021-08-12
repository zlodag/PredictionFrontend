module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http exposing (HttpError(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, toTime, viewData)
import Html exposing (Html, a, b, dd, div, dl, dt, li, text, ul)
import Html.Attributes exposing (href)
import Predictions.Object.Group as Group
import Predictions.Object.User as User
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)



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


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }



-- MODEL


type PossibleData
    = UserList (GraphqlRemoteData UserListResponse)
    | UserData (GraphqlRemoteData UserDetailResponse)
    | GroupList (GraphqlRemoteData GroupListResponse)
    | GroupData (GraphqlRemoteData GroupDetailResponse)
    | NoData


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , data : PossibleData
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url NoData, parseUrlAndRequest url )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResponse PossibleData


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
            ( { model | url = url }
            , parseUrlAndRequest url
            )

        GotResponse graphqlRemoteData ->
            ( { model | data = graphqlRemoteData }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Predictions"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ li [] [ a [ href "/graphql" ] [ text "GraphiQL" ] ]
            , viewLink "/users"
            , viewLink "/groups"
            ]
        , displayData model.data
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


displayMaybe : (a -> Html msg) -> Maybe a -> Html msg
displayMaybe displayFunction data =
    case data of
        Just a ->
            displayFunction a

        Nothing ->
            div [] [ text "Not found" ]


displayData : PossibleData -> Html msg
displayData possibleData =
    case possibleData of
        UserList graphqlRemoteData ->
            viewData (displayNamedNodeList "/user/") graphqlRemoteData

        UserData graphqlRemoteData ->
            viewData (displayMaybe displayUser) graphqlRemoteData

        GroupList graphqlRemoteData ->
            viewData (displayNamedNodeList "/group/") graphqlRemoteData

        GroupData graphqlRemoteData ->
            viewData (displayMaybe displayGroup) graphqlRemoteData

        NoData ->
            text "No data!"


displayNamedNodeListItem : String -> NamedNodeData -> Html msg
displayNamedNodeListItem base_path node =
    case node.id of
        Id id ->
            li [] [ a [ href <| base_path ++ id ] [ text node.name ] ]


displayNamedNodeList : String -> List NamedNodeData -> Html msg
displayNamedNodeList base_path nodes =
    ul [] <| List.map (displayNamedNodeListItem base_path) nodes


displayUser : UserDetailData -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "Name" ]
        , dd []
            [ case user.node.id of
                Id id ->
                    a [ href <| "/user/" ++ id ] [ text user.node.name ]
            ]
        , dt [] [ text "Created" ]
        , dd [] [ text <| toTime user.created ]
        , dt [] [ text "Groups" ]
        , dd [] [ displayNamedNodeList "/group/" user.groups ]
        ]


displayGroup : GroupDetailData -> Html msg
displayGroup group =
    dl []
        [ dt [] [ text "Name" ]
        , dd []
            [ case group.node.id of
                Id id ->
                    a [ href <| "/group/" ++ id ] [ text group.node.name ]
            ]
        , dt [] [ text "Members" ]
        , dd [] [ displayNamedNodeList "/user/" group.members ]
        ]



-- Parse URL


type Route
    = Users
      --| Blog Int
    | User String
    | Groups
    | Group String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Users <| s "users"
        , map User <| s "user" </> string
        , map Groups <| s "groups"
        , map Group <| s "group" </> string
        ]


parseUrlAndRequest : Url.Url -> Cmd Msg
parseUrlAndRequest url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                Users ->
                    userListQuery |> makeRequest (UserList >> GotResponse)

                User string ->
                    Id string |> userDetailQuery |> makeRequest (UserData >> GotResponse)

                Groups ->
                    groupListQuery |> makeRequest (GroupList >> GotResponse)

                Group string ->
                    Id string |> groupDetailQuery |> makeRequest (GroupData >> GotResponse)

        Nothing ->
            Cmd.none



-- Fetch


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserDetailData =
    { node : NamedNodeData
    , created : Timestamp
    , groups : List NamedNodeData
    }


mapToUserDetailData =
    SelectionSet.map3 UserDetailData
        (SelectionSet.map2 NamedNodeData User.id User.name)
        User.created
        (User.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name)


type alias UserDetailResponse =
    Maybe UserDetailData


userDetailQuery : Id -> SelectionSet UserDetailResponse RootQuery
userDetailQuery id =
    Query.user { id = id } <| mapToUserDetailData


type alias UserListResponse =
    List NamedNodeData


userListQuery : SelectionSet UserListResponse RootQuery
userListQuery =
    Query.users <| SelectionSet.map2 NamedNodeData User.id User.name


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    }


mapToGroupDetailData =
    SelectionSet.map2 GroupDetailData
        (SelectionSet.map2 NamedNodeData Group.id Group.name)
        (Group.members <| SelectionSet.map2 NamedNodeData User.id User.name)


type alias GroupDetailResponse =
    Maybe GroupDetailData


groupDetailQuery : Id -> SelectionSet GroupDetailResponse RootQuery
groupDetailQuery id =
    Query.group { id = id } <| mapToGroupDetailData


type alias GroupListResponse =
    List NamedNodeData


groupListQuery : SelectionSet GroupListResponse RootQuery
groupListQuery =
    Query.groups <| SelectionSet.map2 NamedNodeData Group.id Group.name


makeRequest : (GraphqlRemoteData decodesTo -> Msg) -> SelectionSet decodesTo RootQuery -> Cmd Msg
makeRequest msgConstructor set =
    set
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> msgConstructor)
