module Types exposing (Status(..), Model, Problem(..))

import List
import Http
import Routine exposing (..)

type Status = Loading
            | Failure Problem
            | SuccessList RoutineList
            | View Routine
            | Run Routine



type Problem = LoadingError | ParsingError String


-- make opaque, own module, to have runfunction exc defined there and not here

type alias Model = { routines : RoutineListMaybe, status : Status }
