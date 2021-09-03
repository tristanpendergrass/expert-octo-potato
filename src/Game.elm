module Game exposing (..)


type alias Buildings =
    { meadows : Int
    , smiths : Int
    }


type alias Round =
    ( Int, Int )


type alias Game =
    { phase : Phase
    , buildings : Buildings
    , money : Int
    , round : Round
    }


type Phase
    = RollPhase
    | BuyPhase
