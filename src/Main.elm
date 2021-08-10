module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http exposing (HttpError(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helper exposing (viewData, toTime)
import Html exposing (Html, a, b, dd, dl, dt, li, text, ul)
import Html.Attributes exposing (href)
import Predictions.Object.User as User
import Predictions.Query as Query
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String
import Url



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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , data : ModelData
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url RemoteData.Loading, makeRequest )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResponse ModelData


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
            ( { model | url = url }
            , Cmd.none
            )

        GotResponse response ->
            ( { model | data = response }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            , viewLink "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs"
            ]
        , viewData displayUser model.data
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


displayUser : DisplayedUser -> Html msg
displayUser user =
    dl []
        [ dt [] [ text "user.id" ]
        , dd []
            [ case user.id of
                Id id ->
                    text id
            ]
        , dt [] [ text "user.name" ]
        , dd [] [ text user.name ]
        , dt [] [ text "user.created" ]
        , dd [] [ text <| toTime user.created ]
        ]



-- Fetch


type alias Response =
    Maybe DisplayedUser


type alias DisplayedUser =
    { name : String
    , id : Id
    , created : Timestamp
    }


query : Id -> SelectionSet Response RootQuery
query id =
    Query.user { id = id } <|
        SelectionSet.map3 DisplayedUser
            User.name
            User.id
            User.created


type alias ModelData =
    RemoteData (Graphql.Http.Error Response) Response


makeRequest : Cmd Msg
makeRequest =
    Id "53fd525c-c6bf-45a9-b92c-826aeff3baa8"
        |> query
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
