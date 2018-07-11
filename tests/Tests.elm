module Tests exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
    describe "Rover"
        [ describe "initial rover"
            [ test "faces north" <|
                \_ ->
                    Expect.equal initialRover.direction North
            , test "is positioned at (0,0)" <|
                \_ ->
                    Expect.equal initialRover.position <| Pos 0 0
            , describe "applying left"
                [ test "once, faces rover west without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Left ] initialRover
                        in
                            Expect.equal rover (Rover West <| Pos 0 0)
                , test "twice, faces rover south without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Left, Left ] initialRover
                        in
                            Expect.equal rover (Rover South <| Pos 0 0)
                , test "five times, faces rover west without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Left, Left, Left, Left, Left ] initialRover
                        in
                            Expect.equal rover (Rover West <| Pos 0 0)
                ]
            , describe "applying right"
                [ test "once, faces rover east without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right ] initialRover
                        in
                            Expect.equal rover (Rover East <| Pos 0 0)
                , test "twice, faces rover south without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right, Right ] initialRover
                        in
                            Expect.equal rover (Rover South <| Pos 0 0)
                , test "five times, faces rover east without changing position" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right, Right, Right, Right, Right ] initialRover
                        in
                            Expect.equal rover (Rover East <| Pos 0 0)
                ]
            ]
        ]


initialRover : Rover
initialRover =
    Rover North <| Pos 0 0


takeCommand : Command -> Rover -> Rover
takeCommand cmd rover =
    case cmd of
        Left ->
            let
                newDirection =
                    turnLeft rover.direction
            in
                { rover | direction = newDirection }

        Right ->
            let
                newDirection =
                    turnRight rover.direction
            in
                { rover | direction = newDirection }


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds


type Command
    = Left
    | Right


type alias Rover =
    { direction : Direction
    , position : Pos
    }


type alias Pos =
    { x : Int, y : Int }


type Direction
    = North
    | East
    | South
    | West


turnLeft : Direction -> Direction
turnLeft dir =
    case dir of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North



{-
   exampleTests : Test
   exampleTests =
       describe "Sample Test Suite"
           [ describe "Unit test examples"
               [ test "Addition" <|
                   \() ->
                       Expect.equal (3 + 7) 10
               , test "String.left" <|
                   \() ->
                       Expect.equal "a" (String.left 1 "abcdefg")
               ]
           , describe "Fuzz test examples, using randomly generated input"
               [ fuzz (list int) "Lists always have positive length" <|
                   \aList ->
                       List.length aList |> Expect.atLeast 0
               , fuzz (list int) "Sorting a list does not change its length" <|
                   \aList ->
                       List.sort aList |> List.length |> Expect.equal (List.length aList)
               , fuzzWith { runs = 1000 } int "List.member will get an integer in a list containing it" <|
                   \i ->
                       List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
               , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                   \s1 s2 ->
                       s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
               ]
           ]
-}
