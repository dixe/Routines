module Types exposing (Status(..), Model, Problem(..), updateTick)

import List
import Http
import Routine exposing (..)
import Time


type Status = Loading
            | Failure Problem
            | SuccessList RoutineList
            | View Routine
            | Run { routine : Routine, elapsed : Int}



type Problem = LoadingError | ParsingError String


-- make opaque, own module, to have runfunction exc defined there and not here

type alias Model = { routines : RoutineListMaybe, status : Status }


-- UPDATE FUNCTIONS

updateTick : Status -> Status
updateTick s =
    case s of
        Run r -> Run { r | elapsed = r.elapsed  + 1 }
        _ -> s
