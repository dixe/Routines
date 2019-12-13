module Exercise exposing (Exercise, getExerciseName, viewRoutineExercises, createRepsAndSet, createRest, createComplex)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Message exposing(..)



-- atm just name, maybe we also want description
type Exercise = Rest Int
              | TimeAndSet { name : String
                           , sets : Int
                           , restTime : Int
                           , activeTime : Int
                           }
              | RepsAndSet { name : String
                           , sets : Int
                           , restTime : Int
                           , reps : Int
                           }
              | Complex { name : String
                        , sets : Int
                        , restTime : Int
                        , roundsPerSet : Int
                        , exercises : List Exercise
                        }



-- GET AND UPDATE
getExerciseName : Exercise -> String
getExerciseName ex =
    case ex of
        Rest time -> "Rest " ++ String.fromInt time
        RepsAndSet e -> "Reps and set :" ++ e.name
        _ -> "Not a rest"


-- VIEW


viewRoutineExercises : List Exercise -> Element Msg
viewRoutineExercises es = column [spacing 10] (List.map2 viewExercise (addSubNumbers [] (List.length es))  es)

viewExercise : List Int ->  Exercise -> Element Msg
viewExercise index e =
    column []
        [
         case e of
             Rest time -> showExerciseName index ("Rest - " ++ String.fromInt time)
             TimeAndSet t -> showExerciseName index t.name
             RepsAndSet r -> showExerciseName index r.name
             Complex c -> column [spacing 10] [showExerciseName index c.name, column [spacing 10,  paddingXY 10 0]  (List.map2 viewExercise (addSubNumbers index (List.length c.exercises))  c.exercises)]
        ]


addSubNumbers : List Int -> Int -> List (List Int)
addSubNumbers index len = List.map (\x-> index ++ [x]) (List.range 1 len)

     -- given [1,3] 5 produce [[1,3,1],[1,3,2], [1,3,3],[1,3,4],[1,3,5]]
                           --(List.range 1 (List.length c.exercises)
showExerciseName : List Int -> String -> Element Msg
showExerciseName  index name = text ((showIndex index) ++ ": " ++ name)

showIndex : List Int -> String
showIndex index = foldlDefault (\acc x -> x ++ "." ++ acc) "" (List.map String.fromInt index) -- Why is it reversed x ++ acc and not acc ++ x???


--UTILITY --TODO move to list util

foldlDefault : (a->a->a) -> a -> List a -> a
foldlDefault f default l = case l of
                               [] -> default
                               (x::xs) -> List.foldl f x xs


-- CREATERS

createRepsAndSet : String -> Int -> Int -> Int -> Exercise
createRepsAndSet name reps restTime sets = RepsAndSet {name = name, reps = reps, restTime = restTime, sets = sets}

createRest : Int -> Exercise
createRest time = Rest time


createComplex : String -> Int -> Int -> Int -> (List Exercise) ->  Exercise
createComplex name sets restTime rounds exercises = Complex {name=name
                                                            ,  sets = sets
                                                            ,  restTime = restTime
                                                            , roundsPerSet = rounds
                                                            , exercises = exercises}
