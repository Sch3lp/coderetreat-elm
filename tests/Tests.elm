module Tests exposing (suite)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)


eastFacingRover =
    Rover ( 0, 0 ) (Facing East)


southFacingRover =
    Rover ( 0, 0 ) (Facing South)


westFacingRover =
    Rover ( 0, 0 ) (Facing West)


suite : Test
suite =
    describe "Main"
        [ describe "A default Rover"
            [ test "starts at 0,0" <|
                \_ ->
                    defaultRover
                        |> .position
                        |> Expect.equal ( 0, 0 )
            , test "faces North" <|
                \_ ->
                    defaultRover
                        |> .facing
                        |> Expect.equal (Facing North)
            ]
        , describe "A Rover facing North"
            [ describe "Move Forwards command"
                [ test "moves up on y axis" <|
                    \_ ->
                        receiveCommand (Move Forwards) defaultRover
                            |> Expect.equal (Rover ( 0, 1 ) (Facing North))
                , test "twice moves up on y axis twice" <|
                    \_ ->
                        receiveCommands [ Move Forwards, Move Forwards ] defaultRover
                            |> Expect.equal (Rover ( 0, 2 ) (Facing North))
                ]
            , describe "Move Backwards command"
                [ test "moves down on y axis" <|
                    \_ ->
                        receiveCommand (Move Backwards) defaultRover
                            |> Expect.equal (Rover ( 0, -1 ) (Facing North))
                , test "twice moves down on y axis twice" <|
                    \_ ->
                        receiveCommands [ Move Backwards, Move Backwards ] defaultRover
                            |> Expect.equal (Rover ( 0, -2 ) (Facing North))
                ]
            , describe "Rotate Right command"
                [ test "faces East" <|
                    \_ ->
                        receiveCommand (Rotate Right) defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing East))
                , test "two times faces South" <|
                    \_ ->
                        receiveCommands [ Rotate Right, Rotate Right ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing South))
                , test "three times faces West" <|
                    \_ ->
                        receiveCommands [ Rotate Right, Rotate Right, Rotate Right ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing West))
                , test "four times faces back North" <|
                    \_ ->
                        receiveCommands [ Rotate Right, Rotate Right, Rotate Right, Rotate Right ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing North))
                ]
            , describe "Rotate Left command"
                [ test "faces West" <|
                    \_ ->
                        receiveCommand (Rotate Left) defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing West))
                , test "two times faces South" <|
                    \_ ->
                        receiveCommands [ Rotate Left, Rotate Left ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing South))
                , test "three times faces East" <|
                    \_ ->
                        receiveCommands [ Rotate Left, Rotate Left, Rotate Left ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing East))
                , test "four times faces back North" <|
                    \_ ->
                        receiveCommands [ Rotate Left, Rotate Left, Rotate Left, Rotate Left ] defaultRover
                            |> Expect.equal (Rover ( 0, 0 ) (Facing North))
                ]
            ]
        , describe "A Rover facing East"
            [ describe "Move Forwards command"
                [ test "moves up on x axis" <|
                    \_ ->
                        receiveCommand (Move Forwards) eastFacingRover
                            |> Expect.equal (Rover ( 1, 0 ) (Facing East))
                , test "twice moves up on x axis twice" <|
                    \_ ->
                        receiveCommands [ Move Forwards, Move Forwards ] eastFacingRover
                            |> Expect.equal (Rover ( 2, 0 ) (Facing East))
                ]
            , describe "Move Backwards command"
                [ test "moves down on x axis" <|
                    \_ ->
                        receiveCommand (Move Backwards) eastFacingRover
                            |> Expect.equal (Rover ( -1, 0 ) (Facing East))
                , test "twice moves down on x axis twice" <|
                    \_ ->
                        receiveCommands [ Move Backwards, Move Backwards ] eastFacingRover
                            |> Expect.equal (Rover ( -2, 0 ) (Facing East))
                ]
            ]
        , describe "A Rover facing South"
            [ describe "Move Forwards command"
                [ test "moves down on y axis" <|
                    \_ ->
                        receiveCommand (Move Forwards) southFacingRover
                            |> Expect.equal (Rover ( 0, -1 ) (Facing South))
                , test "twice moves down on y axis twice" <|
                    \_ ->
                        receiveCommands [ Move Forwards, Move Forwards ] southFacingRover
                            |> Expect.equal (Rover ( 0, -2 ) (Facing South))
                ]
            , describe "Move Backwards command"
                [ test "moves down up y axis" <|
                    \_ ->
                        receiveCommand (Move Backwards) southFacingRover
                            |> Expect.equal (Rover ( 0, 1 ) (Facing South))
                , test "twice moves down up y axis twice" <|
                    \_ ->
                        receiveCommands [ Move Backwards, Move Backwards ] southFacingRover
                            |> Expect.equal (Rover ( 0, 2 ) (Facing South))
                ]
            ]
        , describe "A Rover facing West"
            [ describe "Move Forwards command"
                [ test "moves down on x axis" <|
                    \_ ->
                        receiveCommand (Move Forwards) westFacingRover
                            |> Expect.equal (Rover ( -1, 0 ) (Facing West))
                , test "twice moves down on x axis twice" <|
                    \_ ->
                        receiveCommands [ Move Forwards, Move Forwards ] westFacingRover
                            |> Expect.equal (Rover ( -2, 0 ) (Facing West))
                ]
            , describe "Move Backwards command"
                [ test "moves down up x axis" <|
                    \_ ->
                        receiveCommand (Move Backwards) westFacingRover
                            |> Expect.equal (Rover ( 1, 0 ) (Facing West))
                , test "twice moves down up x axis twice" <|
                    \_ ->
                        receiveCommands [ Move Backwards, Move Backwards ] westFacingRover
                            |> Expect.equal (Rover ( 2, 0 ) (Facing West))
                ]
            ]
        , test "A Rover adventures" <|
            \_ ->
                let
                    cmds =
                        [ Move Forwards
                        , Rotate Right
                        , Move Forwards
                        , Rotate Right
                        , Move Forwards
                        , Move Forwards
                        , Rotate Left
                        , Move Backwards
                        , Move Backwards
                        , Move Backwards
                        ]
                in
                receiveCommands cmds defaultRover
                    |> Expect.equal (Rover ( -2, -1 ) (Facing East))
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
