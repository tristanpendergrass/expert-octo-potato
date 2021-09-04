module Game exposing (..)


type Building
    = Meadow
    | Smith


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


type alias BuyOptions =
    ( Building, Building )


type Phase
    = RollPhase
    | BuyPhase BuyOptions
