module CaseList exposing (Data, initialQuery, queryRequest, view)

import Common exposing (NamedNodeData, Now, caseUrl, displayNamedNode)
import Config exposing (api)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html, div, label, li, option, select, text, ul)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Predictions.Object.Case
import Predictions.Object.User
import Predictions.Query
import Predictions.Scalar exposing (Id(..))


type Data
    = Data CaseListData


type alias CaseListData =
    { userId : Id
    , creator : OptionalArgument Bool
    , tag : OptionalArgument String
    , tags : List String
    , cases : List NamedNodeData
    }


initialQuery : Id -> Graphql.Http.Request Data
initialQuery userId =
    let
        creator =
            Absent

        tag =
            Absent
    in
    SelectionSet.map2 NamedNodeData Predictions.Object.Case.id Predictions.Object.Case.reference
        |> Predictions.Object.User.cases (\args -> { args | creator = creator, tag = tag })
        |> SelectionSet.map2 (CaseListData userId creator tag) Predictions.Object.User.tags
        |> Predictions.Query.user { id = userId }
        |> SelectionSet.map Data
        |> Graphql.Http.queryRequest api


queryRequest : Data -> Graphql.Http.Request Data
queryRequest (Data data) =
    SelectionSet.map2 NamedNodeData Predictions.Object.Case.id Predictions.Object.Case.reference
        |> Predictions.Object.User.cases (\args -> { args | creator = data.creator, tag = data.tag })
        |> SelectionSet.map (\cases -> { data | cases = cases })
        |> Predictions.Query.user { id = data.userId }
        |> SelectionSet.map Data
        |> Graphql.Http.queryRequest api


view : (Data -> msg) -> Data -> Html msg
view requestNewList (Data data) =
    let
        onChangeTag : String -> msg
        onChangeTag value =
            requestNewList <|
                Data
                    { data
                        | tag =
                            if value == allTags then
                                Absent

                            else
                                Present value
                    }

        onChangeCreator : String -> msg
        onChangeCreator value =
            requestNewList <|
                Data
                    { data
                        | creator =
                            if value == creatorStrings.me then
                                Present True

                            else if value == creatorStrings.others then
                                Present False

                            else
                                Absent
                    }

        creatorStrings =
            { me = "me"
            , others = "others"
            }

        allTags =
            "_all"
    in
    div []
        [ div []
            [ label [] [ text "Tag: " ]
            , select [ onInput onChangeTag ] <| option [ value allTags, selected (data.tag == Absent) ] [ text "(All)" ] :: List.map (\tag -> option [ value tag, selected (data.tag == Present tag) ] [ text tag ]) data.tags
            ]
        , div []
            [ label [] [ text "Creator: " ]
            , select [ onInput onChangeCreator ]
                [ option [ value "", selected (data.creator == Absent) ] [ text "(All)" ]
                , option [ value creatorStrings.me, selected (data.creator == Present True) ] [ text "Me" ]
                , option [ value creatorStrings.others, selected (data.creator == Present False) ] [ text "Others" ]
                ]
            ]
        , ul [] <| List.map (displayNamedNode caseUrl >> List.singleton >> li []) data.cases
        ]
