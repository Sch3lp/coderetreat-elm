module Position exposing (..)


type alias Pos =
    { x : Int, y : Int }


type alias AxisDirection =
    Int -> Int


up : AxisDirection
up =
    (\i -> i)


down : AxisDirection
down =
    (\i -> i * -1)


moveY : AxisDirection -> Pos -> Pos
moveY dir pos =
    { pos | y = pos.y + (dir 1) }


moveX : AxisDirection -> Pos -> Pos
moveX dir pos =
    { pos | x = pos.x + (dir 1) }
