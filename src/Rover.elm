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


marsRover : Rover
marsRover =
    Rover North <| PlanetPos mars <| Pos 0 0


marsPos : Pos -> PlanetPos
marsPos pos =
    PlanetPos mars pos


mars : Planet
mars =
    Planet "Mars" 16


aiur : Planet
aiur =
    Planet "Aiur" 20


aiurPos : Pos -> PlanetPos
aiurPos pos =
    PlanetPos aiur pos



{- todo: obstacles' Planet and model's planet (or Rovers' planet for that matter), can be two different planets -}


type alias Model =
    { rover : Rover
    , planet : Planet
    , obstacles : List Obstacle
    }


initialModel : Model
initialModel =
    Model marsRover mars obstaclesOnMars


obstaclesOnMars : List Obstacle
obstaclesOnMars =
    [ Crater <| marsPos <| Pos -1 0
    , Crater <| marsPos <| Pos 6 -1
    , Debris <| marsPos <| Pos 0 1
    , Teleport (marsPos <| Pos 0 -7) (aiurPos <| Pos 0 0)
    ]



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
