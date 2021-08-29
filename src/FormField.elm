module FormField exposing (Field, displayValidity, getValue, newField, newNonEmptyStringField, onInput, withValue)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (style, value)
import Html.Events


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
displayValidity field =
    let
        ( backgroundColor, validationText ) =
            case field of
                Field fieldData ->
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
