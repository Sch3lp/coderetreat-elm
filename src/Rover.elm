module Rover exposing (..)

import Position exposing (..)
import Planet exposing (..)


type Command
    = Left
    | Right
    | Forward
    | Backward


type Aim
    = North
    | East
    | South
    | West


type alias Heading =
    Aim


headingAsString : Heading -> String
headingAsString heading =
    case heading of
        North ->
            "North"

        East ->
            "East"

        West ->
            "West"

        South ->
            "South"


type alias Rover =
    { aim : Aim
    , heading : Heading
    , message : Maybe String
    , position : PlanetPos
    }


marsRover : Rover
marsRover =
    Rover North North Nothing <| marsPos <| Pos 0 0


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
    [ Crater <| marsPos <| Pos -3 0
    , Crater <| marsPos <| Pos 6 -1
    , Debris <| marsPos <| Pos 1 3
    , Teleport (marsPos <| Pos 2 6) (aiurPos <| Pos 0 0)
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
            { rover | aim = turnLeft rover.aim }

        Right ->
            { rover | aim = turnRight rover.aim }

        Forward ->
            move (moveConsideringObstacles obstaclesOnMars rover moveForward) rover

        Backward ->
            move (moveConsideringObstacles obstaclesOnMars rover moveBackward) rover


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds


move : CheckedMoveAction -> Rover -> Rover
move checkedMoveAction rover =
    case checkedMoveAction of
        Unobstructed moveAction ->
            moveAction rover

        Obstructed msg ->
            { rover | message = Just msg }


moveConsideringObstacles : List Obstacle -> Rover -> MoveAction -> CheckedMoveAction
moveConsideringObstacles obstacles rover moveAction =
    let
        wouldHitObstacle =
            scanForObstacles obstacles (moveAction rover) |> .message
    in
        case wouldHitObstacle of
            Just msg ->
                Obstructed msg

            Nothing ->
                Unobstructed moveAction


type CheckedMoveAction
    = Obstructed String
    | Unobstructed MoveAction


type alias MoveAction =
    Rover -> Rover



-- scanning for obstacles


scanForObstacles : List Obstacle -> Rover -> Rover
scanForObstacles obstacles rover =
    let
        obstacleFound : Maybe String
        obstacleFound =
            List.filterMap (positionHasObstacle rover.position) obstacles
                |> List.head
    in
        case obstacleFound of
            Just msg ->
                { rover | message = formatMsgWithDirection rover.heading msg }

            Nothing ->
                rover


formatMsgWithDirection : Heading -> String -> Maybe String
formatMsgWithDirection heading msg =
    let
        concatenatedMsg =
            String.join " " [ msg, headingAsString heading, "of me and cannot move in that direction." ]
    in
        Just concatenatedMsg


positionHasObstacle : PlanetPos -> Obstacle -> Maybe String
positionHasObstacle pos obstacle =
    case obstacle of
        Crater obstaclePos ->
            if obstaclePos == pos then
                Just "I found a crater"
            else
                Nothing

        Debris obstaclePos ->
            if obstaclePos == pos then
                Just "I found debris"
            else
                Nothing

        Teleport obstaclePos warpPos ->
            if obstaclePos == pos then
                Just "I found a teleport"
            else
                Nothing



-- moving


moveForward : MoveAction
moveForward rover =
    case rover.aim of
        North ->
            { rover | position = moveYAndWrap up rover.position, heading = North }

        East ->
            { rover | position = moveXAndWrap up rover.position, heading = East }

        South ->
            { rover | position = moveYAndWrap down rover.position, heading = South }

        West ->
            { rover | position = moveXAndWrap down rover.position, heading = West }


moveBackward : MoveAction
moveBackward rover =
    case rover.aim of
        North ->
            { rover | position = moveYAndWrap down rover.position, heading = South }

        East ->
            { rover | position = moveXAndWrap down rover.position, heading = West }

        South ->
            { rover | position = moveYAndWrap up rover.position, heading = North }

        West ->
            { rover | position = moveXAndWrap up rover.position, heading = East }



-- turning


turnLeft : Aim -> Aim
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


turnRight : Aim -> Aim
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
