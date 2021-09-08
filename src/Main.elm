module Main exposing (main)

import Browser
import Browser.Events
import Dice exposing (Dice(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Building
    = Meadow
    | Smith


type Phase
    = RollPhase Dice
    | BuildPhase Building Building


type alias Model =
    { seed : Random.Seed
    , phase : Phase
    , money : Int
    , buildings : List Building
    , roundPanelIsOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 0
      , phase = RollPhase Dice.create
      , money = 0
      , buildings = [ Meadow, Smith ]
      , roundPanelIsOpen = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = HandleAnimationFrameDelta Float
    | Roll
    | ToggleRoundPanel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case ( msg, model.phase ) of
        ( Roll, _ ) ->
            let
                ( newDice, newSeed ) =
                    Random.step Dice.roll model.seed

                newModel =
                    { model
                        | seed = newSeed
                        , phase = RollPhase newDice
                    }
            in
            ( newModel, Cmd.none )

        ( HandleAnimationFrameDelta delta, RollPhase dice ) ->
            let
                newMoney =
                    case Dice.numberWasRolled delta dice of
                        Just Dice.Two ->
                            model.money + 1

                        _ ->
                            model.money

                newDice =
                    Dice.handleAnimationFrameDelta delta dice

                newModel =
                    { model
                        | phase = RollPhase newDice
                        , money = newMoney
                    }
            in
            ( newModel, Cmd.none )

        ( ToggleRoundPanel, _ ) ->
            ( { model | roundPanelIsOpen = not model.roundPanelIsOpen }, Cmd.none )

        _ ->
            noOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta



-- VIEW


stack : Html Msg
stack =
    node "stack-l" [] []


renderBuildings : List Building -> Html Msg
renderBuildings buildings =
    div [] []


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen p-6 bg-gray-900 text-gray-100 flex space-x-4" ]
        [ case model.phase of
            RollPhase dice ->
                div [ class "w-64 h-72 flex-col justify-center items-center space-y-4 relative" ]
                    [ div [ class "flex justify-center w-full" ]
                        [ button
                            [ class "bg-blue-800 hover:bg-blue-700 active:bg-blue-600 cursor-pointer rounded shadow py-1 px-4"
                            , onClick Roll
                            ]
                            [ text "Roll" ]
                        ]
                    , Dice.render dice
                    , div [ class "absolute bottom-0 left-0 h-20 w-full bg-gray-500" ] []
                    ]

            _ ->
                div [] []
        , div [ class "flex-grow h-72 flex justify-start items-center space-x-4" ]
            [ div [ class "h-full flex-grow" ] [ renderBuildings model.buildings ]
            , div [ class "h-full w-24 flex justify-center items-center" ] [ span [ class "text-6xl" ] [ text <| "$" ++ String.fromInt model.money ] ]
            ]
        ]
