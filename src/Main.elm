module Main exposing (Command(..), Direction(..), Facing(..), MoveCommand(..), RotateCommand(..), Rover, defaultRover, receiveCommand, receiveCommands)


type Direction
    = North
    | East
    | South
    | West


type Facing
    = Facing Direction


type alias Rover =
    { position : ( Int, Int ), facing : Facing }


type MoveCommand
    = Forwards
    | Backwards


type RotateCommand
    = Right
    | Left


type Command
    = Move MoveCommand
    | Rotate RotateCommand


defaultRover : Rover
defaultRover =
    Rover ( 0, 0 ) (Facing North)


receiveCommands : List Command -> Rover -> Rover
receiveCommands cmds rover =
    List.foldl receiveCommand rover cmds


receiveCommand : Command -> Rover -> Rover
receiveCommand cmd rover =
    case cmd of
        Move a ->
            move a rover

        Rotate a ->
            rotate a rover


move : MoveCommand -> Rover -> Rover
move cmd rover =
    let
        axis =
            determineAxis rover.facing

        axisDirection =
            determineAxisDirection rover.facing

        modifiedAxisDirection =
            if cmd == Backwards then
                flipAxisDirection axisDirection

            else
                axisDirection

        newPosition =
            axis (apply modifiedAxisDirection) rover.position
    in
    { rover | position = newPosition }


rotate : RotateCommand -> Rover -> Rover
rotate cmd rover =
    let
        newFacing =
            case cmd of
                Right ->
                    rotateRight rover.facing

                Left ->
                    rotateLeft rover.facing
    in
    { rover | facing = newFacing }


rotateRight facing =
    case facing of
        Facing North ->
            Facing East

        Facing East ->
            Facing South

        Facing South ->
            Facing West

        Facing West ->
            Facing North


rotateLeft facing =
    case facing of
        Facing North ->
            Facing West

        Facing West ->
            Facing South

        Facing South ->
            Facing East

        Facing East ->
            Facing North


determineAxis direction =
    case direction of
        Facing North ->
            Tuple.mapSecond

        Facing East ->
            Tuple.mapFirst

        Facing South ->
            Tuple.mapSecond

        Facing West ->
            Tuple.mapFirst


determineAxisDirection direction =
    case direction of
        Facing North ->
            Up

        Facing East ->
            Up

        Facing South ->
            Down

        Facing West ->
            Down


type AxisDirection
    = Up
    | Down


flipAxisDirection : AxisDirection -> AxisDirection
flipAxisDirection axisDirection =
    case axisDirection of
        Up ->
            Down

        Down ->
            Up


apply : AxisDirection -> (Int -> Int)
apply axisDirection =
    case axisDirection of
        Up ->
            increment

        Down ->
            decrement


decrement : Int -> Int
decrement a =
    a - 1


increment : Int -> Int
increment a =
    a + 1
