module Main exposing (..)


type Command
    = Left
    | Right
    | Forward
    | Backward


type alias Rover =
    { direction : Direction
    , position : Pos
    }


type alias Pos =
    { x : Int, y : Int }


type alias Planet =
    { name : String
    , size : Int
    }


type Direction
    = North
    | East
    | South
    | West


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

        Backward ->
            { rover | position = moveBackward rover }


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds


edges : Planet -> List Pos
edges p =
    let
        edge =
            p.size // 2

        inverse =
            (\i -> i * -1)

        edge1 =
            Pos (inverse edge) (inverse edge)

        edge2 =
            Pos (edge) (edge)

        edge3 =
            Pos (inverse edge) (edge)

        edge4 =
            Pos (edge) (inverse edge)
    in
        [ edge1, edge2, edge3, edge4 ]


moveForward : Rover -> Pos
moveForward rover =
    case rover.direction of
        North ->
            moveY up rover.position

        East ->
            moveX up rover.position

        South ->
            moveY down rover.position

        West ->
            moveX down rover.position


moveBackward : Rover -> Pos
moveBackward rover =
    case rover.direction of
        North ->
            moveY down rover.position

        East ->
            moveX down rover.position

        South ->
            moveY up rover.position

        West ->
            moveX up rover.position


up =
    (\i -> i)


down =
    (\i -> i * -1)


moveY dir pos =
    { pos | y = pos.y + (dir 1) }


moveX dir pos =
    { pos | x = pos.x + (dir 1) }


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
