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
    , message : Maybe String
    , position : PlanetPos
    }


marsRover : Rover
marsRover =
    Rover North Nothing <| PlanetPos mars <| Pos 0 0


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
    , Debris <| marsPos <| Pos 0 2
    , Teleport (marsPos <| Pos 0 7) (aiurPos <| Pos 0 0)
    ]



--helper


positionedAt : Rover -> Pos
positionedAt rover =
    rover.position.pos


getMessage : Rover -> Maybe String
getMessage rover =
    rover.message



-- commands


takeCommand : Command -> Rover -> Rover
takeCommand cmd rover =
    case cmd of
        Left ->
            Debug.log "Left" <| { rover | direction = turnLeft rover.direction }

        Right ->
            Debug.log "Right" <| { rover | direction = turnRight rover.direction }

        Forward ->
            Debug.log "Forward" <| (moveForward >> scanForObstacles obstaclesOnMars) rover

        Backward ->
            Debug.log "Backward" <| (moveBackward >> scanForObstacles obstaclesOnMars) rover


takeCommands : List Command -> Rover -> Rover
takeCommands cmds rover =
    List.foldl takeCommand rover cmds


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
                { rover | message = formatMsgWithDirection rover.direction msg }

            Nothing ->
                rover


formatMsgWithDirection : Direction -> String -> Maybe String
formatMsgWithDirection dir msg =
    let
        dirAsString =
            case dir of
                North ->
                    "North"

                East ->
                    "East"

                West ->
                    "West"

                South ->
                    "South"

        concatenatedMsg =
            String.join " " [ msg, dirAsString, "of me and cannot move in that direction." ]
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


moveForward : Rover -> Rover
moveForward rover =
    case rover.direction of
        North ->
            { rover | position = moveYAndWrap up rover.position }

        East ->
            { rover | position = moveXAndWrap up rover.position }

        South ->
            { rover | position = moveYAndWrap down rover.position }

        West ->
            { rover | position = moveXAndWrap down rover.position }


moveBackward : Rover -> Rover
moveBackward rover =
    case rover.direction of
        North ->
            { rover | position = moveYAndWrap down rover.position }

        East ->
            { rover | position = moveXAndWrap down rover.position }

        South ->
            { rover | position = moveYAndWrap up rover.position }

        West ->
            { rover | position = moveXAndWrap up rover.position }



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
