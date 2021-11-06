module Config exposing (api)

import Url.Builder


api : String
api =
    Url.Builder.absolute [ "api" ] []
