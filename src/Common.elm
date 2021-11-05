module Common exposing (..)

import Html exposing (Html, a, span, text)
import Html.Attributes exposing (href, title)
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.Scalar exposing (Id(..))
import Time exposing (Month(..), Weekday(..))
import Time.Distance
import Url.Builder


type alias Now =
    { zone : Time.Zone
    , posix : Time.Posix
    }


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias UserInfo =
    { node : NamedNodeData
    , accessToken : String
    , refreshToken : String
    }


displayNamedNode : (Id -> String) -> NamedNodeData -> Html msg
displayNamedNode toUrl node =
    a [ node.id |> toUrl |> href ] [ text node.name ]


userUrl : Id -> String
userUrl (Id id) =
    Url.Builder.absolute [ "users", id ] []


groupUrl : Id -> String
groupUrl (Id id) =
    Url.Builder.absolute [ "groups", id ] []


caseUrl : Id -> String
caseUrl (Id id) =
    Url.Builder.absolute [ "cases", id ] []


displayTime : Now -> Time.Posix -> Html msg
displayTime now timeToDisplay =
    span [ title <| getTimeString now.zone timeToDisplay ] [ text <| " " ++ Time.Distance.inWords timeToDisplay now.posix ]


getTimeString : Time.Zone -> Time.Posix -> String
getTimeString zone time =
    toWeekday (Time.toWeekday zone time)
        ++ " "
        -- DD
        ++ toPaddedString 2 (Time.toDay zone time)
        ++ "/"
        -- MM
        ++ toPaddedString 2 (fromMonth (Time.toMonth zone time))
        ++ "/"
        -- YYYY
        ++ toPaddedString 4 (Time.toYear zone time)
        ++ " "
        -- HH
        ++ toPaddedString 2 (Time.toHour zone time)
        ++ ":"
        -- mm
        ++ toPaddedString 2 (Time.toMinute zone time)
        ++ ":"
        -- ss
        ++ toPaddedString 2 (Time.toSecond zone time)


toWeekday : Weekday -> String
toWeekday weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


displayOutcomeSymbol : Outcome -> String
displayOutcomeSymbol outcome =
    case outcome of
        Right ->
            "✔"

        Wrong ->
            "✗"

        Indeterminate ->
            "?"


getShortDateString : Time.Zone -> Time.Posix -> String
getShortDateString zone time =
    -- DD
    toPaddedString 2 (Time.toDay zone time)
        ++ "/"
        -- MM
        ++ toPaddedString 2 (fromMonth <| Time.toMonth zone time)
        ++ "/"
        -- YY
        ++ toPaddedString 2 (remainderBy 100 <| Time.toYear zone time)


toPaddedString : Int -> Int -> String
toPaddedString digits time =
    String.padLeft digits '0' (String.fromInt time)


fromMonth : Time.Month -> Int
fromMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
