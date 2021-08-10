module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http exposing (HttpError(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (GraphqlRemoteData, toTime, viewData)
import Html exposing (Html, a, b, dd, div, dl, dt, li, text, ul)
import Html.Attributes exposing (href)
import Predictions.Object.User as User
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String
import Url
import Url.Parser exposing (Parser, (</>), map, oneOf, s, string)



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
    | GotUserListResponse (GraphqlRemoteData UserListResponse)
    | GotUserDetailResponse (GraphqlRemoteData UserDetailResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, data = NoData }
            , parseUrlAndRequest url
            )

        GotUserDetailResponse graphqlRemoteData ->
            ( { model | data = UserData graphqlRemoteData }
            , Cmd.none
            )

        GotUserListResponse graphqlRemoteData ->
            ( { model | data = UserList graphqlRemoteData }
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
            [ viewLink "/src/Main.elm"
            , viewLink "/users"
            , viewLink "/groups"
            ]
        , displayData model.data
        ]
    }

displayData: PossibleData -> Html msg
displayData possibleData =
    case possibleData of
        UserData data ->
            viewData (displayMaybe displayUser) data

        UserList data ->
            viewData displayUserList data

        NoData ->
            text "No data!"

viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]

displayUserList : List UserDetailData -> Html msg
displayUserList list =
    let
        makeItem: UserDetailData -> Html msg
        makeItem detail =
            li [] [displayUser detail]
    in

        ul [] <| List.map makeItem list

displayMaybe : (a -> Html msg) -> Maybe a -> Html msg
displayMaybe displayFunction data =
    case data of
        Just a ->
            displayFunction a
        Nothing ->
            div [] [text "Not found"]

displayUser : UserDetailData -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "Name" ]
        , dd [] [ case user.id of
                Id id ->
                    a [href <| "/user/" ++ id] [text user.name]
            ]
        , dt [] [ text "Created" ]
        , dd [] [ text <| toTime user.created ]
        ]

-- Parse URL


type Route
  = Users
  --| Blog Int
  | User String
  --| Comment String Int

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Users <| s "users"
    --, map Blog    (s "blog" </> int)
    , map User <| s "user" </> string
    --, map Comment (s "user" </> string </> s "comment" </> int)
    ]

-- /topic/pottery        ==>  Just (Topic "pottery")
-- /topic/collage        ==>  Just (Topic "collage")
-- /topic/               ==>  Nothing

-- /blog/42              ==>  Just (Blog 42)
-- /blog/123             ==>  Just (Blog 123)
-- /blog/mosaic          ==>  Nothing

-- /user/tom/            ==>  Just (User "tom")
-- /user/sue/            ==>  Just (User "sue")
-- /user/bob/comment/42  ==>  Just (Comment "bob" 42)
-- /user/sam/comment/35  ==>  Just (Comment "sam" 35)
-- /user/sam/comment/    ==>  Nothing
-- /user/                ==>  Nothing



parseUrlAndRequest: Url.Url -> Cmd Msg
parseUrlAndRequest url =
    case Url.Parser.parse routeParser url of
        Just route ->
            case route of
                User string ->
                   Id string |> userDetailQuery |> makeRequest GotUserDetailResponse

                Users ->
                    userListQuery |> makeRequest GotUserListResponse


        Nothing -> Cmd.none


-- Fetch


-- User

mapToUserDetailData =
    SelectionSet.map3 UserDetailData
                User.name
                User.id
                User.created

type alias UserDetailData =
    { name : String
    , id : Id
    , created : Timestamp
    }

type alias UserDetailResponse =
    Maybe UserDetailData

userDetailQuery : Id -> SelectionSet UserDetailResponse RootQuery
userDetailQuery id =
    Query.user { id = id } <| mapToUserDetailData

type alias UserListResponse =
    List UserDetailData

userListQuery: SelectionSet UserListResponse RootQuery
userListQuery =
    Query.users <| mapToUserDetailData


makeRequest : (GraphqlRemoteData decodesTo -> Msg) -> SelectionSet decodesTo RootQuery -> Cmd Msg
makeRequest msgConstructor set =
    set
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> msgConstructor)
