module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Graphql.Http.GraphqlError exposing (GraphqlError, Location, PossiblyParsedData(..))
import Html exposing (Html, a, b, dd, div, dl, dt, li, pre, text, ul)
import Html.Attributes exposing (href)
import List exposing (map)
import Predictions.Scalar exposing (Id(..), Timestamp(..))
import RemoteData exposing (RemoteData)
import String exposing (fromInt, toInt)
import Url
import Http
import Time exposing (Posix)
import Iso8601 as Iso

import Graphql.Http exposing (HttpError(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Predictions.Query as Query
import Predictions.Object.User as User

import Json.Decode as Decode

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
    { title: String,
     body : List (Html msg)
 }

-- MODEL

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , response : ModelValue
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key url RemoteData.Loading, makeRequest )

-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotResponse ModelValue

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
            ( { model | response = response }
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
      , viewResponse model.response
      ]
  }



viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]

viewResponse : ModelValue -> Html msg
viewResponse response =
    case response of
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
                                Just user ->
                                    div [] [viewUser user, text "Http.GraphqlError with ParsedData" |> viewWithGraphqlErrors graphqlErrors]
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
                            div [] [ text "Graphql.Http.HttpError: BadPayload", pre [] [ text <| Decode.errorToString error ]]
        RemoteData.Success d ->
            case d of
                Just user ->
                    viewUser user
                Nothing ->
                    text "RemoteData.Success with empty ParsedData"

viewHeaders: Dict String String -> List (Html msg)
viewHeaders headers =
    Dict.foldl addHeader [] headers

addHeader: String -> String -> List (Html msg) -> List (Html msg)
addHeader key value headers =
     headers ++ [dt [] [text key], dd [] [text value]]

viewMetadata: Http.Metadata -> Html msg
viewMetadata metadata =
    dl []
        [ dt [] [text "url"]
        , dd [] [text metadata.url]
        , dt [] [text "statusCode"]
        , dd [] [text <| fromInt metadata.statusCode]
        , dt [] [text "statusText"]
        , dd [] [text metadata.statusText]
        , dt [] [text "headers"]
        , dd [] (viewHeaders metadata.headers)
    ]

viewGraphqlErrorLocation: Location -> Html msg
viewGraphqlErrorLocation location =
    dl []
        [ dt [] [text "line"]
        , dd [] [text <| fromInt location.line]
        , dt [] [text "column"]
        , dd [] [text <| fromInt location.column]
    ]

viewGraphqlError : GraphqlError -> Html msg
viewGraphqlError error =
    li []
        [ dl []
            [ dt [] [text "error.message"]
            , dd [] [text error.message]
            , dt [] [text "error.locations"]
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
        Timestamp string -> case string |> toInt of
            Just millis ->
                millis  |> Time.millisToPosix |> Iso.fromTime
            Nothing ->
                "Non-integer timestamp"

viewUser : DisplayedUser -> Html msg
viewUser user =
    dl []
        [ dt [] [ text "user.id" ]
        , dd [] [ case user.id of
            Id id -> text id
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

type alias ModelValue =
    RemoteData (Graphql.Http.Error Response) Response

makeRequest: Cmd Msg
makeRequest =
    Id "53fd525c-c6bf-45a9-b92c-826aeff3baa8"
        |> query
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)