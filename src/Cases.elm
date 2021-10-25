module Cases exposing (..)

import Helper exposing (GraphqlRemoteData, NamedNodeData, Now, displayNamedNodeList)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (type_, value)
import Predictions.Scalar exposing (Id)


type alias Params =
    { userId : Id
    , tag : Maybe String
    }


type alias Cases =
    GraphqlRemoteData (List NamedNodeData)


displayCaseList : Params -> List NamedNodeData -> Html msg
displayCaseList params cases =
    div []
        [ div []
            [ label [] [ text "Tag: " ]
            , input [ type_ "text", value <| Maybe.withDefault "" params.tag ] []
            ]
        , displayNamedNodeList [ "case" ] cases
        ]
