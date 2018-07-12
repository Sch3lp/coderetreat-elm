module Tests exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


middleOfMars =
    PlanetPos (Pos 0 0) mars


suite : Test
suite =
    describe "Rover"
        [ describe "initial rover"
            [ test "faces north" <|
                \_ ->
                    initialRover.direction
                        |> Expect.equal North
            , test "is positioned at (0,0)" <|
                \_ ->
                    initialRover.position.pos
                        |> Expect.equal (Pos 0 0)
            , describe "applying left"
                [ test "once, faces rover west without changing position" <|
                    \_ ->
                        takeCommands [ Left ] initialRover
                            |> Expect.equal { initialRover | direction = West }
                , test "twice, faces rover south without changing position" <|
                    \_ ->
                        takeCommands [ Left, Left ] initialRover
                            |> Expect.equal { initialRover | direction = South }
                , test "five times, faces rover west without changing position" <|
                    \_ ->
                        takeCommands [ Left, Left, Left, Left, Left ] initialRover
                            |> Expect.equal { initialRover | direction = West }
                ]
            , describe "applying right"
                [ test "once, faces rover east without changing position" <|
                    \_ ->
                        takeCommands [ Right ] initialRover
                            |> Expect.equal { initialRover | direction = East }
                , test "twice, faces rover south without changing position" <|
                    \_ ->
                        takeCommands [ Right, Right ] initialRover
                            |> Expect.equal { initialRover | direction = South }
                , test "five times, faces rover east without changing position" <|
                    \_ ->
                        takeCommands [ Right, Right, Right, Right, Right ] initialRover
                            |> Expect.equal { initialRover | direction = East }
                ]
            , describe "applying right and left"
                [ test "twice right, twice left, faces north again" <|
                    \_ ->
                        takeCommands [ Right, Right, Left, Left ] initialRover
                            |> Expect.equal { initialRover | direction = North }
                , test "twice right, twice left alternating, faces north again" <|
                    \_ ->
                        takeCommands [ Right, Left, Right, Left ] initialRover
                            |> Expect.equal { initialRover | direction = North }
                , test "right right right right left, faces west" <|
                    \_ ->
                        takeCommands [ Right, Right, Right, Right, Left ] initialRover
                            |> Expect.equal { initialRover | direction = West }
                ]
            , describe "applying forward"
                [ test "once, moves to (0,1)" <|
                    \_ ->
                        takeCommands [ Forward ] initialRover
                            |> positionedAt
                            |> Expect.equal (Pos 0 1)
                , test "twice, moves to (0,2)" <|
                    \_ ->
                        takeCommands [ Forward, Forward ] initialRover
                            |> positionedAt
                            |> Expect.equal (Pos 0 2)
                , describe "facing east"
                    [ test "once, moves to (1,0)" <|
                        \_ ->
                            takeCommands [ Forward ] (Rover East <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos 1 0)
                    , test "twice, moves to (2,0)" <|
                        \_ ->
                            takeCommands [ Forward, Forward ] (Rover East <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos 2 0)
                    ]
                , describe "facing south"
                    [ test "once, moves to (0,-1)" <|
                        \_ ->
                            takeCommands [ Forward ] (Rover South <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos 0 -1)
                    , test "twice, moves to (0,-2)" <|
                        \_ ->
                            takeCommands [ Forward, Forward ] (Rover South <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos 0 -2)
                    ]
                , describe "facing west"
                    [ test "once, moves to (-1,0)" <|
                        \_ ->
                            takeCommands [ Forward ] (Rover West <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos -1 0)
                    , test "twice, moves to (0,-2)" <|
                        \_ ->
                            takeCommands [ Forward, Forward ] (Rover West <| middleOfMars)
                                |> positionedAt
                                |> Expect.equal (Pos -2 0)
                    ]
                ]
            , describe "applying backward"
                [ test "once, moves to (0,-1)" <|
                    \_ ->
                        takeCommands [ Backward ] initialRover
                            |> positionedAt
                            |> Expect.equal (Pos 0 -1)
                , test "twice, moves to (0,-22)" <|
                    \_ ->
                        takeCommands [ Backward, Backward ] initialRover
                            |> positionedAt
                            |> Expect.equal (Pos 0 -2)
                ]
            , describe "various commands"
                [ test "left forward forward left forward" <|
                    \_ ->
                        takeCommands [ Left, Forward, Forward, Left, Forward ] initialRover
                            |> Expect.equal (Rover South <| marsPos -2 -1)
                , test "left forward backward right forward" <|
                    \_ ->
                        takeCommands [ Left, Forward, Backward, Right, Forward ] initialRover
                            |> Expect.equal (Rover North <| marsPos 0 1)
                , test "commands that cause wrapping" <|
                    \_ ->
                        takeCommands [ Forward, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Right, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Forward ] initialRover
                            |> Expect.equal (Rover East <| marsPos -7 -7)
                ]
            ]
        ]


marsPos : Int -> Int -> PlanetPos
marsPos x y =
    PlanetPos (Pos x y) mars


moon =
    Planet "moon" 4


planet : Test
planet =
    describe "Planet"
        [ describe "edges"
            [ test "moon has 4x4, so edges at (-1,-1) to (1,1)" <|
                \_ ->
                    edges moon
                        |> Expect.equal [ Pos -1 -1, Pos 1 1, Pos -1 1, Pos 1 -1 ]
            , test "mars has 16x16, so edges at (-8,-8) to (8,8)" <|
                \_ ->
                    edges mars
                        |> Expect.equal [ Pos -7 -7, Pos 7 7, Pos -7 7, Pos 7 -7 ]
            ]
        , describe "moving out of bounds wraps to the other side"
            [ test "at (-1,-1) moving west results in (1,-1)" <|
                \_ ->
                    PlanetPos (Pos -1 -1) moon
                        |> moveXAndWrap down
                        |> Expect.equal
                            (PlanetPos (Pos 1 -1) moon)
            , test "at (1,1) moving east results in (-1,1)" <|
                \_ ->
                    PlanetPos (Pos 1 1) moon
                        |> moveXAndWrap up
                        |> Expect.equal
                            (PlanetPos (Pos -1 1) moon)
            , test "at (1,1) moving north results in (1,-1)" <|
                \_ ->
                    PlanetPos (Pos 1 1) moon
                        |> moveYAndWrap up
                        |> Expect.equal
                            (PlanetPos (Pos 1 -1) moon)
            , test "at (-1,-1) moving south results in (-1,1)" <|
                \_ ->
                    PlanetPos (Pos -1 -1) moon
                        |> moveYAndWrap down
                        |> Expect.equal
                            (PlanetPos (Pos -1 1) moon)
            ]
        ]



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
