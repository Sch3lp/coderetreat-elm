module Rover exposing (..)

import Position exposing (..)
import Planet exposing (..)


type Command
    = Left
    | Right
    | Forward
    | Backward


type Direction
    = North
    | East
    | South
    | West


type alias Rover =
    { direction : Direction
    , position : PlanetPos
    }


initialRover : Rover
initialRover =
    Rover North <| PlanetPos (Pos 0 0) mars


mars =
    Planet "Mars" 16



--helper


positionedAt : Rover -> Pos
positionedAt rover =
    rover.position.pos



-- commands


takeCommand : Command -> Rover -> Rover
takeCommand cmd rover =
    case cmd of
        Left ->
            { rover | direction = turnLeft rover.direction }

        Right ->
            { rover | direction = turnRight rover.direction }

        Forward ->
            { rover | position = moveForward rover }

        Backward ->
            { rover | position = moveBackward rover }


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds



-- moving


moveForward : Rover -> PlanetPos
moveForward rover =
    case rover.direction of
        North ->
            moveYAndWrap up rover.position

        East ->
            moveXAndWrap up rover.position

        South ->
            moveYAndWrap down rover.position

        West ->
            moveXAndWrap down rover.position


moveBackward : Rover -> PlanetPos
moveBackward rover =
    case rover.direction of
        North ->
            moveYAndWrap down rover.position

        East ->
            moveXAndWrap down rover.position

        South ->
            moveYAndWrap up rover.position

        West ->
            moveXAndWrap up rover.position



-- turning


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
