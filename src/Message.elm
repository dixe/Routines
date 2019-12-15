module Message exposing (Msg(..))

import Http exposing (..)
import Time


type Msg
    = GotAll (Result Http.Error String)
    | GotSingle (Result Http.Error String)
    | FetchAll
    | FetchOne Int
    | Filter String
    | StartRoutine
    | Tick
