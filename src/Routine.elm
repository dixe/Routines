module Routine exposing(Routine, RoutineListMaybe, RoutineList, RoutineInfo, getName, getId, getExercises, getNameInfo, getIdInfo, createRoutineInfo, createRoutine, getRoutineList, createRoutineListMaybe, setFilterString, getRoutineListFiltered, getFilterValue, updateTick, getCurrentName, finishSet)

import Exercise as Ex exposing (..)

type alias Routine =  { id : Int
                      , name : String
                      , elapsed : Int
                      , completed : List Ex.Exercise
                      , current : Maybe Ex.Exercise
                      , remaining : List Ex.Exercise
                      }


type RoutineInfo = RoutineInfo { id : Int
                               , name : String
                               }


type alias RoutineList =  List RoutineInfo


type alias RoutineListMaybe = Maybe {routines : List RoutineInfo, nameFilter : String}



--HELPER

filterRoutine : String -> RoutineInfo -> Bool
filterRoutine s ri = String.contains (String.toLower s) (String.toLower (getNameInfo ri))



-- GETTER

getName : Routine -> String
getName r = r.name

getId : Routine -> Int
getId r = r.id

getExercises : Routine -> List Ex.Exercise
getExercises r =
    let currentList  =
            case r.current of
                Just c -> [c]
                Nothing -> []
    in
        r.completed ++ currentList ++ r.remaining

getNameInfo : RoutineInfo -> String
getNameInfo (RoutineInfo r) = r.name

getIdInfo : RoutineInfo -> Int
getIdInfo (RoutineInfo r) = r.id


getRoutineListFiltered : RoutineListMaybe -> List RoutineInfo
getRoutineListFiltered rm =
    case rm of
        Nothing -> []
        Just ri -> List.filter (filterRoutine ri.nameFilter) ri.routines

getRoutineList : RoutineListMaybe -> List RoutineInfo
getRoutineList rm =
    case rm of
        Nothing -> []
        Just ri -> ri.routines


getFilterValue : RoutineListMaybe -> String
getFilterValue rlm =
    case rlm of
        Just r -> r.nameFilter
        Nothing -> ""

getCurrentName : Routine -> String
getCurrentName r = case r.current of
                       Nothing -> "All is done"
                       Just e -> Ex.getExerciseName e



-- CREATE FUNCTIONS

createRoutine : Int -> String -> (List Ex.Exercise) -> Routine
createRoutine id name exs =
    case exs of
        [] -> {id = id, name = name, remaining = exs, elapsed = 0, current = Nothing, completed = []}
        (e::es) -> {id = id, name = name, remaining = es, elapsed = 0, current = Just e, completed = []}


createRoutineInfo : Int -> String -> RoutineInfo
createRoutineInfo id name = RoutineInfo { id = id, name = name }


createRoutineListMaybe : List RoutineInfo -> RoutineListMaybe
createRoutineListMaybe rl = Just  { routines = rl, nameFilter = ""}


-- UPDATE FUNCTIONS

setFilterString : String -> RoutineListMaybe -> RoutineListMaybe
setFilterString s rm =
    case rm of
        Just r -> Just {r | nameFilter = s}
        Nothing -> rm


updateTick : Routine -> Routine
updateTick r =
    let timeUpdated = { r | elapsed = r.elapsed + 1}
    in
        case timeUpdated.current of
            Just e ->
                case Ex.updateTick timeUpdated.elapsed e of
                    Just eu  -> { timeUpdated | current = Just eu}
                    Nothing -> nextExercise timeUpdated
            Nothing ->  nextExercise timeUpdated



nextExercise : Routine -> Routine
nextExercise r =
    let currentList  =
            case r.current of
                Just c -> [c]
                Nothing -> []
    in
        case r.remaining of
            [] -> { r | current = Nothing
                  , completed = r.completed ++ currentList
                  }
            (e::es) -> { r | remaining = es
                       , completed = r.completed ++ currentList
                       , current = Just e
                       , elapsed = 0
                       }


finishSet : Routine -> Routine
finishSet r =
    case r.current of
        Just e -> { r | current = Ex.finishSet r.current }
        Nothing -> r
