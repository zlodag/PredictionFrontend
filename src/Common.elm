module Common exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Cases
import Chart.Item as CI
import Dict
import FormField exposing (Field)
import Graphql.Http
import Html exposing (Html)
import NewCase
import PredictionList
import Predictions.Enum.Outcome exposing (Outcome(..))
import Predictions.InputObject exposing (CaseInput, CommentInput, PredictionInput)
import Predictions.Scalar exposing (Id(..))
import RemoteData exposing (RemoteData(..), WebData)
import ScalarCodecs exposing (Id, Timestamp)
import String
import Time
import Url


type alias Model =
    { key : Nav.Key
    , now : Now
    , state : State
    }


type State
    = SignedOut (Maybe (Html Msg)) Credentials
    | SigningIn Credentials
    | SignedIn User Data


type Data
    = NoData
    | UserDetail (GraphqlRemoteData UserDetailData)
    | ScoreDetail (List (CI.One Score CI.Dot)) (GraphqlRemoteData (List Score))
    | PredictionList PredictionList.Params PredictionList.Predictions
    | CaseList Cases.Params Cases.Cases
    | EventList (GraphqlRemoteData (List EventResult))
    | GroupList (GraphqlRemoteData (List NamedNodeData))
    | GroupDetail (GraphqlRemoteData GroupDetailData)
    | CaseDetail (GraphqlRemoteData CaseDetailData)
    | NewCase (Maybe String) NewCase.Data
    | ImportFromPB ImportParams (Maybe Id) (RemoteData (Html Msg) ImportedData)


type Msg
    = NewTime Time.Zone Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotState State
    | SignIn Credentials
    | SubmitCase NewCase.Data CaseInput
    | SubmitComment CaseDetailData String
    | SubmitDeadline CaseDetailData Timestamp
    | SubmitGroup CaseDetailData (Maybe Id)
    | SubmitDiagnosis CaseDetailData PredictionInput
    | SubmitWager CaseDetailData Id Int
    | SubmitJudgement CaseDetailData Id Outcome
    | CaseCreated Id
    | ChangeImportParams ImportParams
    | ImportData ImportParams
    | GotImportedDataResult ImportParams (RemoteData (Html Msg) ImportedData)
    | GotDecrypted (List EncryptedReference)
    | ImportToDatabase (List CaseInput)
    | CasesImported Int
    | FetchPredictions PredictionList.Params
    | GotPredictions PredictionList.Params PredictionList.Predictions


type alias Now =
    { zone : Time.Zone
    , posix : Time.Posix
    }


type alias GraphqlRemoteData a =
    RemoteData (Graphql.Http.Error a) a


type alias Credentials =
    { username : String
    , password : String
    }


blankCredentials =
    { username = ""
    , password = ""
    }


type alias NamedNodeData =
    { id : Id
    , name : String
    }


type alias User =
    { node : NamedNodeData
    , token : String
    }


type alias UserDetailData =
    { node : NamedNodeData
    , created : Timestamp
    , score : Maybe Float
    , groups : List NamedNodeData
    , casesCreated : List NamedNodeData
    , tags : List String
    }


type alias GroupDetailData =
    { node : NamedNodeData
    , members : List NamedNodeData
    , cases : List NamedNodeData
    }


type alias CaseDetailData =
    { state : CaseDetailState
    , node : NamedNodeData
    , creator : NamedNodeData
    , group : Maybe NamedNodeData
    , deadline : Timestamp
    , diagnoses : List DiagnosisDetailData
    , comments : List CommentData
    , tags : List String
    }


type CaseDetailState
    = Viewing
    | ChangingDeadline (Field Timestamp)
    | ChangingGroup (List NamedNodeData)
    | AddingDiagnosis PredictionData
    | AddingWager Id (Field Int)
    | Judging Id
    | AddingComment (Field String)


type alias PredictionData =
    { diagnosis : Field String
    , confidence : Field Int
    }


type alias DiagnosisDetailData =
    { node : NamedNodeData
    , wagers : List WagerData
    , judgement : Maybe JudgementData
    }


type alias WagerData =
    { creator : NamedNodeData
    , confidence : Int
    , timestamp : Timestamp
    }


type alias JudgementData =
    { judgedBy : NamedNodeData
    , timestamp : Timestamp
    , outcome : Outcome
    }


type alias CommentData =
    { creator : NamedNodeData
    , timestamp : Timestamp
    , text : String
    }


type alias Score =
    { judged : Timestamp
    , case_ : NamedNodeData
    , diagnosis : String
    , confidence : Int
    , outcome : Outcome
    , brierScore : Float
    , averageBrierScore : Float
    , adjustedBrierScore : Float
    }


type EventResult
    = WagerActivity ActivityData String Int
    | JudgementActivity ActivityData String Outcome
    | CommentActivity ActivityData String
    | GroupCaseActivity ActivityData NamedNodeData
    | DeadlineEvent EventData


type alias ActivityData =
    { event : EventData
    , user : NamedNodeData
    }


type alias EventData =
    { case_ : NamedNodeData
    , timestamp : Timestamp
    }


type alias ImportParams =
    { apiToken : String
    , pageSize : Int
    , page : Int
    , password : String
    }


type alias ImportedData =
    { user : ImportedUser
    , predictions : ImportedCases
    }


type alias ImportedUser =
    { name : String
    , email : String
    , userId : Int
    }


type alias ImportedCases =
    { count : Int
    , cases : Dict.Dict Int ImportedCase
    }


type alias ImportedCase =
    { reference : EncryptedReference
    , created : Timestamp
    , deadline : Timestamp
    , diagnoses : List ImportedDiagnosis
    , comments : List ImportedComment
    , selected : Bool
    }


type alias EncryptedReference =
    { groupId : Int
    , ciphertext : String
    , cleartext : Maybe String
    }


type alias ImportedDiagnosis =
    { diagnosis : String
    , confidence : Int
    , outcome : Maybe Outcome
    }


type alias ImportedComment =
    { timestamp : Timestamp
    , text : String
    }
