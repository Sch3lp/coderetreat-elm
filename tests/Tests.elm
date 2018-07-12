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
            , describe "applying right and left"
                [ test "twice right, twice left, faces north again" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right, Right, Left, Left ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 0)
                , test "twice right, twice left alternating, faces north again" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right, Left, Right, Left ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 0)
                , test "right right right right left, faces west" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Right, Right, Right, Right, Left ] initialRover
                        in
                            Expect.equal rover (Rover West <| Pos 0 0)
                ]
            , describe "applying forward"
                [ test "once, moves to (0,1)" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Forward ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 1)
                , test "twice, moves to (0,2)" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Forward, Forward ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 2)
                , describe "facing east"
                    [ test "once, moves to (1,0)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward ] (Rover East <| Pos 0 0)
                            in
                                Expect.equal rover (Rover East <| Pos 1 0)
                    , test "twice, moves to (2,0)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward, Forward ] (Rover East <| Pos 0 0)
                            in
                                Expect.equal rover (Rover East <| Pos 2 0)
                    ]
                , describe "facing south"
                    [ test "once, moves to (0,-1)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward ] (Rover South <| Pos 0 0)
                            in
                                Expect.equal rover (Rover South <| Pos 0 -1)
                    , test "twice, moves to (0,-2)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward, Forward ] (Rover South <| Pos 0 0)
                            in
                                Expect.equal rover (Rover South <| Pos 0 -2)
                    ]
                , describe "facing west"
                    [ test "once, moves to (-1,0)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward ] (Rover West <| Pos 0 0)
                            in
                                Expect.equal rover (Rover West <| Pos -1 0)
                    , test "twice, moves to (0,-2)" <|
                        \_ ->
                            let
                                rover =
                                    takeCommands [ Forward, Forward ] (Rover West <| Pos 0 0)
                            in
                                Expect.equal rover (Rover West <| Pos -2 0)
                    ]
                ]
            , describe "applying backward"
                [ test "once, moves to (0,-1)" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Backward ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 -1)
                , test "twice, moves to (0,-22)" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Backward, Backward ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 -2)
                ]
            , describe "various commands"
                [ test "left forward forward left forward" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Left, Forward, Forward, Left, Forward ] initialRover
                        in
                            Expect.equal rover (Rover South <| Pos -2 -1)
                , test "left forward backward right forward" <|
                    \_ ->
                        let
                            rover =
                                takeCommands [ Left, Forward, Backward, Right, Forward ] initialRover
                        in
                            Expect.equal rover (Rover North <| Pos 0 1)
                ]
            ]
        ]


moon =
    Planet "moon" 4


mars =
    Planet "mars" 16


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
                    moveXAndWrap down { pos = (Pos -1 -1), planet = moon }
                        |> Expect.equal { pos = (Pos 1 -1), planet = moon }
            ]
        ]


type alias PlanetPos =
    { pos : Pos, planet : Planet }


moveXAndWrap : AxisDirection -> PlanetPos -> PlanetPos
moveXAndWrap dir planetPos =
    let
        currentPos =
            planetPos.pos

        newPos =
            if wouldMoveOutOfBounds dir planetPos then
                { currentPos | x = (currentPos.x * -1) }
            else
                moveX dir currentPos
    in
        { planetPos | pos = newPos }


wouldMoveOutOfBounds : AxisDirection -> PlanetPos -> Bool
wouldMoveOutOfBounds dir planetPos =
    let
        currentPos =
            planetPos.pos

        newPos =
            moveX dir currentPos
    in
        List.any (\edgePos -> (abs newPos.x) > (abs edgePos.x)) (edges planetPos.planet)



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
