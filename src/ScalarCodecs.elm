module ScalarCodecs exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Predictions.Scalar exposing (defaultCodecs)
import Time


type alias Id =
    Predictions.Scalar.Id


type alias Timestamp =
    Time.Posix


codecs : Predictions.Scalar.Codecs Id Timestamp
codecs =
    Predictions.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        , codecTimestamp =
            { encoder = \posixTime -> posixTime |> Time.posixToMillis |> Encode.int
            , decoder = Decode.int |> Decode.map Time.millisToPosix
            }
        }
