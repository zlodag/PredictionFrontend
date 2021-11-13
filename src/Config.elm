module Config exposing (api, privateGroupName)

import Url.Builder


api : String
api =
    Url.Builder.absolute [ "api" ] []


privateGroupName : String
privateGroupName =
    "(Private)"
