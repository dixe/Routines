module Message exposing (Msg(..))

import Http exposing (..)

type Msg
    = GotAll (Result Http.Error String)
    | GotSingle (Result Http.Error String)
    | FetchAll
    | FetchOne Int
    | Filter String
