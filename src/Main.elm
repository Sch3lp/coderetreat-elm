module Main exposing (..)


initialRover : Rover
initialRover =
    Rover North <| Pos 0 0


takeCommand : Command -> Rover -> Rover
takeCommand cmd rover =
    case cmd of
        Left ->
            { rover | direction = turnLeft rover.direction }

        Right ->
            { rover | direction = turnRight rover.direction }

        Forward ->
            { rover | position = moveForward rover }


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds


type Command
    = Left
    | Right
    | Forward


type alias Rover =
    { direction : Direction
    , position : Pos
    }


type alias Pos =
    { x : Int, y : Int }


moveForward : Rover -> Pos
moveForward rover =
    case rover.direction of
        North ->
            moveY up rover.position

        East ->
            moveX rover.position

        South ->
            moveY down rover.position

        _ ->
            rover.position


up =
    (\i -> i)


down =
    (\i -> i * -1)


moveY dir pos =
    { pos | y = pos.y + (dir 1) }


moveX pos =
    { pos | x = pos.x + 1 }


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
