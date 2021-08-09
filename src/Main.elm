module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (ul, li, text, a, b, Html)
import Html.Attributes exposing (href)
import Url

import Url.Parser exposing (Parser, parse, (</>), map, top, oneOf, s, string)

type Route
    = UserList
  --= Topic String
  --| Blog Int
  | User String
  --| Comment String Int

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map UserList (s "users" </> top)
    --[ map Topic   (s "topic" </> string)
    --, map Blog    (s "blog" </> int)
    , map User    (s "user" </> string)
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


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , route : Maybe Route
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key url (parse routeParser url), Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


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
      ( { model | url = url, route = (parse routeParser url) }
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
          [ viewLink "/src/Main.elm"
          , viewLink "/users/"
          , viewLink "/user/the-century-of-the-self"
          , viewLink "/user/public-opinion"
          , viewLink "/user/shah-of-shahs"
          ]
      , parseStatus model.route
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

parseStatus : Maybe Route -> Html msg
parseStatus route =
    case route of
        Nothing ->
            text "No route was found"
        Just UserList ->
            text "We have a route: users!"
        Just (User u) ->
            text ("Welcome, " ++ u)


