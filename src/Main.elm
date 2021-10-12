module Main exposing (main)

import Browser
import Browser.Events
import Dice exposing (Dice(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Shop
    = Shop Building Building


type Building
    = Meadow
    | Smith


type alias Buildings =
    { meadows : Int
    , smiths : Int
    }


type Round
    = RoundOne
    | RoundTwo
    | RoundThree
    | RoundFour


type Phase
    = RollOnePhase
    | RollTwoPhase
    | RollThreePhase
    | BuyPhase
    | PayRentPhase


type alias Model =
    { seed : Random.Seed
    , dice : Dice
    , money : Int
    , buildings : Buildings
    , round : Round
    , phase : Phase
    , shop : Maybe Shop
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 0
      , dice = Dice.create
      , money = 0
      , buildings = { meadows = 1, smiths = 2 }
      , round = RoundOne
      , phase = RollOnePhase
      , shop = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = HandleAnimationFrameDelta Float
    | Roll
    | SkipToBuy
    | Buy Building
    | SkipBuy


isRollPhase : Model -> Bool
isRollPhase model =
    model.phase == RollOnePhase || model.phase == RollTwoPhase || model.phase == RollThreePhase


nextRound : Model -> Model
nextRound model =
    let
        newRound =
            case model.round of
                RoundOne ->
                    RoundTwo

                RoundTwo ->
                    RoundThree

                RoundThree ->
                    RoundFour

                RoundFour ->
                    RoundFour
    in
    { model
        | round = newRound
        , phase = RollOnePhase
        , dice = Dice.create
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        Roll ->
            if isRollPhase model then
                let
                    ( newDice, newSeed ) =
                        Random.step Dice.roll model.seed

                    newModel =
                        { model
                            | seed = newSeed
                            , dice = newDice
                        }
                in
                ( newModel, Cmd.none )

            else
                noOp

        SkipToBuy ->
            let
                newModel =
                    { model | phase = BuyPhase, shop = Just (Shop Meadow Smith) }
            in
            ( newModel, Cmd.none )

        Buy building ->
            let
                oldBuildings =
                    model.buildings

                newMoney =
                    model.money - 1

                canBuy =
                    newMoney >= 0

                newModel =
                    case building of
                        Meadow ->
                            { model | buildings = { oldBuildings | meadows = model.buildings.meadows + 1 }, money = newMoney }

                        Smith ->
                            { model | buildings = { oldBuildings | smiths = model.buildings.smiths + 1 }, money = newMoney }
            in
            if canBuy then
                ( nextRound newModel, Cmd.none )

            else
                noOp

        SkipBuy ->
            case model.phase of
                BuyPhase ->
                    ( nextRound model, Cmd.none )

                _ ->
                    noOp

        HandleAnimationFrameDelta delta ->
            let
                newMoney =
                    case Dice.numberWasRolled model.dice of
                        Just Dice.Two ->
                            model.money + (4 * model.buildings.meadows)

                        Just _ ->
                            model.money + (1 * model.buildings.smiths)

                        _ ->
                            model.money

                newPhase =
                    if Maybe.Extra.isJust (Dice.numberWasRolled model.dice) then
                        case model.phase of
                            RollOnePhase ->
                                RollTwoPhase

                            RollTwoPhase ->
                                RollThreePhase

                            RollThreePhase ->
                                BuyPhase

                            BuyPhase ->
                                model.phase

                            PayRentPhase ->
                                model.phase

                    else
                        model.phase

                newDice =
                    Dice.handleAnimationFrameDelta delta model.dice

                newShop =
                    if Maybe.Extra.isJust (Dice.numberWasRolled model.dice) then
                        Just (Shop Meadow Smith)

                    else
                        model.shop

                newModel =
                    { model
                        | dice = newDice
                        , money = newMoney
                        , phase = newPhase
                        , shop = newShop
                    }
            in
            ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta



-- VIEW


primaryButton : List (Attribute msg) -> List (Html msg) -> Html msg
primaryButton attrs =
    button <|
        List.concat
            [ [ class "border-2 rounded-border border-gray-100 px-4 py-1 bg-blue-700 hover:bg-blue-600 active:bg-blue-500 focus:outline-none" ]
            , attrs
            ]


secondaryButton : List (Attribute msg) -> List (Html msg) -> Html msg
secondaryButton attrs =
    button <|
        List.concat
            [ [ class "border-b border-dashed border-gray-100 focus:outline-none active:text-gray-400 hover:text-gray-300" ]
            , attrs
            ]


disabledPrimaryButtonClass : String
disabledPrimaryButtonClass =
    "opacity-50 hover:bg-blue-700 active:bg-blue-700"


type alias EveryLayoutEl =
    List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg


stack : EveryLayoutEl
stack =
    node "stack-l"


center : EveryLayoutEl
center =
    node "center-l"


sidebar : EveryLayoutEl
sidebar =
    node "sidebar-l"


box : EveryLayoutEl
box =
    node "box-l"


cover : EveryLayoutEl
cover =
    node "cover-l"


switcher : EveryLayoutEl
switcher =
    node "switcher-l"


renderBuildings : Model -> Html Msg
renderBuildings model =
    div [ class "w-100 h-100 flex space-x-32" ]
        [ div [ class "flex-col items-center justify-center space-y-8" ]
            [ div [ class "flex items-center space-x-4" ]
                [ renderMeadow
                , div [] [ text <| "x " ++ String.fromInt model.buildings.meadows ]
                ]
            , div [ class "flex items-center space-x-4" ]
                [ renderSmith
                , div [] [ text <| "x " ++ String.fromInt model.buildings.smiths ]
                ]
            ]
        ]


renderBuilding : Building -> Html Msg
renderBuilding building =
    case building of
        Meadow ->
            renderMeadow

        Smith ->
            renderSmith


renderMeadow : Html Msg
renderMeadow =
    div [ class "rounded-border-2 border-2 border-gray-100 w-64 h-40 bg-blue-500" ]
        [ stack []
            [ div [ class "flex justify-center items-center p-4 space-x-4" ]
                [ div [ class "rounded-border border-2 border-gray-100 w-12 h-12 bg-green-500" ] []
                , h4 [ class "inline-block text-xl" ] [ text "Meadow" ]
                ]
            , div [ class "flex justify-center items-center p-4 space-x-2" ]
                [ div [ class "w-6 h-6" ] [ Dice.renderDie Dice.Two ]
                , h4 [ class "inline-block text-xl" ] [ text "$4" ]
                ]
            ]
        ]


renderSmith : Html Msg
renderSmith =
    div [ class "rounded-border-2 border-2 border-gray-100 w-64 h-40 bg-yellow-600" ]
        [ stack []
            [ div [ class "flex justify-center items-center p-4 space-x-4" ]
                [ div [ class "rounded-border border-2 border-gray-100 w-12 h-12 bg-red-600" ] []
                , h4 [ class "inline-block text-xl" ] [ text "Smith" ]
                ]
            , div [ class "flex justify-center items-center p-4 space-x-2" ]
                [ div [ class "w-6 h-6" ] [ Dice.renderDie Dice.Two ]
                , h4 [ class "inline-block text-xl" ] [ text "$1" ]
                ]
            ]
        ]


renderRoundPanel : Model -> Html Msg
renderRoundPanel model =
    let
        roundText =
            case model.round of
                RoundOne ->
                    "Round 1 / 4"

                RoundTwo ->
                    "Round 2 / 4"

                RoundThree ->
                    "Round 3 / 4"

                RoundFour ->
                    "Round 4 / 4"

        boldIfPhaseIs : Phase -> String
        boldIfPhaseIs phase =
            if model.phase == phase then
                "font-bold"

            else
                ""
    in
    div [ class "absolute w-full bg-blue-300 top-0 left-0 border-t-2 border-blue-500 transition-all h-16" ]
        [ div [ class "flex items-center w-full h-16 px-4 text-gray-900 space-x-4" ]
            [ div [] [ text roundText ]
            , div [ class "border-l border-dashed border-gray-900 h-3/4" ] []
            , div [ class <| boldIfPhaseIs RollOnePhase ] [ text "Roll" ]
            , div [] [ text ">" ]
            , div [ class <| boldIfPhaseIs RollTwoPhase ] [ text "Roll" ]
            , div [] [ text ">" ]
            , div [ class <| boldIfPhaseIs RollThreePhase ] [ text "Roll" ]
            , div [] [ text ">" ]
            , div [ class <| boldIfPhaseIs BuyPhase ] [ text "Buy" ]
            ]
        ]


renderRollArea : Model -> Html Msg
renderRollArea model =
    let
        disableRollButton =
            not (isRollPhase model) || Dice.isRolling model.dice
    in
    stack
        []
        [ center [ class "flex items-center space-x-4" ]
            [ primaryButton
                [ onClick Roll
                , class "w-20"
                , class <|
                    if disableRollButton then
                        disabledPrimaryButtonClass

                    else
                        ""
                , disabled disableRollButton
                ]
                [ text "Roll" ]
            , secondaryButton
                [ onClick SkipToBuy
                ]
                [ text "Skip to Buy" ]
            ]
        , center
            [ class <|
                if isRollPhase model then
                    ""

                else
                    "opacity-50"
            ]
            [ Dice.render model.dice ]
        ]


getBuildingLabel : Building -> String
getBuildingLabel building =
    case building of
        Meadow ->
            "Meadow"

        Smith ->
            "Smith"


renderBuyArea : Model -> Html Msg
renderBuyArea model =
    case model.shop of
        Nothing ->
            div [] []

        Just (Shop option1 option2) ->
            let
                isBuyDisabled =
                    model.money < 1

                renderBuyOption option =
                    div [ class "flex items-center space-x-16" ]
                        [ renderBuilding option
                        , primaryButton
                            [ class "w-24"
                            , class <|
                                if isBuyDisabled then
                                    disabledPrimaryButtonClass

                                else
                                    ""
                            , onClick (Buy option)
                            , disabled isBuyDisabled
                            ]
                            [ text "Buy" ]
                        ]
            in
            div [ class "flex flex-col items-center space-y-8" ]
                [ div [ class "text-4xl" ] [ text "Pick 1" ]
                , renderBuyOption option1
                , renderBuyOption option2
                , secondaryButton [ onClick SkipBuy ] [ text "Skip" ]
                ]


view : Model -> Html Msg
view model =
    sidebar [ class "h-full ", attribute "sideWidth" "35%" ]
        [ cover [ attribute "centered" ".area", attribute "noPad" "true", class "border-r-4 border-gray-100 border-dotted relative overflow-hidden" ]
            [ renderRoundPanel model
            , div [ class "area h-full w-full overflow-hidden relative" ]
                [ div
                    [ style "width" "200%"
                    , style "left"
                        (if model.phase == BuyPhase then
                            "-100%"

                         else
                            "0"
                        )
                    , class "absolute top-0 flex h-full items-center"
                    ]
                    [ div [ class "w-1/2 h-full flex justify-center items-center" ] [ renderRollArea model ]
                    , div [ class "w-1/2 h-full flex justify-center items-center" ] [ renderBuyArea model ]
                    ]
                ]
            ]
        , cover [ attribute "centered" ".buildings-container" ]
            [ center []
                [ h2 [ class "text-6xl" ] [ text <| "$" ++ String.fromInt model.money ]
                ]
            , center [ class "buildings-container" ]
                [ renderBuildings model
                ]
            ]
        ]
