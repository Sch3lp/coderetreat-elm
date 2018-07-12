module Planet exposing (..)

import Position exposing (..)


type alias Planet =
    { name : String
    , size : Int
    }


type alias PlanetPos =
    { pos : Pos, planet : Planet }


moveXAndWrap : AxisDirection -> PlanetPos -> PlanetPos
moveXAndWrap dir planetPos =
    let
        currentPos =
            planetPos.pos

        moveInDir =
            moveX dir

        newPos =
            if wouldMoveOutOfBounds moveInDir planetPos then
                { currentPos | x = (currentPos.x * -1) }
            else
                moveInDir currentPos
    in
        { planetPos | pos = newPos }


moveYAndWrap : AxisDirection -> PlanetPos -> PlanetPos
moveYAndWrap dir planetPos =
    let
        currentPos =
            planetPos.pos

        moveInDir =
            moveY dir

        newPos =
            if wouldMoveOutOfBounds moveInDir planetPos then
                { currentPos | y = (currentPos.y * -1) }
            else
                moveInDir currentPos
    in
        { planetPos | pos = newPos }


wouldMoveOutOfBounds : (Pos -> Pos) -> PlanetPos -> Bool
wouldMoveOutOfBounds moveDir planetPos =
    let
        currentPos =
            planetPos.pos

        newPos =
            moveDir currentPos
    in
        edges planetPos.planet
            |> List.any (\edgePos -> ((abs newPos.x) > (abs edgePos.x)) || ((abs newPos.y) > (abs edgePos.y)))


edges : Planet -> List Pos
edges p =
    let
        edge =
            (p.size // 2) - 1

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
