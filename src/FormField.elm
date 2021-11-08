module FormField exposing (Field, displayValidity, getValue, newField, newNonEmptyStringField, onInput, validateConfidence, validateDeadline, withValue)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (style, value)
import Html.Events
import Iso8601
import ScalarCodecs exposing (Timestamp)


withValue : Field a -> Attribute msg
withValue field =
    case field of
        Field fieldData ->
            value fieldData.inputValue


onInput : (Field a -> msg) -> Field a -> Attribute msg
onInput listener field =
    Html.Events.onInput <| listener << updateField field


getValue : Field a -> Result String a
getValue field =
    case field of
        Field fieldData ->
            fieldData.value


newField : (String -> Result String a) -> Field a
newField validator =
    Field <| FieldData "" (validator "") validator


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


newNonEmptyStringField : String -> Field String
newNonEmptyStringField label =
    let
        validator : String -> Result String String
        validator string =
            let
                trimmed =
                    String.trim string
            in
            if String.isEmpty trimmed then
                Err <| label ++ " cannot be empty"

            else
                Ok trimmed
    in
    newField validator


updateField : Field a -> String -> Field a
updateField field inputValue =
    case field of
        Field fieldData ->
            Field <| { fieldData | value = fieldData.validator inputValue, inputValue = inputValue }


displayValidity : Field a -> Html msg
displayValidity (Field fieldData) =
    let
        ( backgroundColor, validationText ) =
            case fieldData.value of
                Ok _ ->
                    ( "green", "✓" )

                Err error ->
                    ( "red", "✗ " ++ error )
    in
    span [ style "background-color" backgroundColor, style "color" "white" ] [ text validationText ]


type Field a
    = Field (FieldData a)


type alias FieldData a =
    { inputValue : String
    , value : Result String a
    , validator : String -> Result String a
    }


validateDeadline : String -> Result String Timestamp
validateDeadline deadline =
    let
        errorMapper _ =
            "Could not parse as ISO-8601 datetime string"
    in
    Result.mapError errorMapper <| Iso8601.toTime deadline
