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


type alias Buildings =
    { meadows : Int
    }


type alias Model =
    { seed : Random.Seed
    , dice : Dice
    , money : Int
    , buildings : Buildings
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 0
      , dice = Dice.create
      , money = 0
      , buildings = { meadows = 1 }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = HandleAnimationFrameDelta Float
    | Roll


setDice : Dice -> Model -> Model
setDice dice model =
    { model | dice = dice }


setSeed : Random.Seed -> Model -> Model
setSeed seed model =
    { model | seed = seed }


updateDice : (Dice -> Dice) -> Model -> Model
updateDice fn model =
    { model | dice = fn model.dice }


setMoney : Int -> Model -> Model
setMoney newValue model =
    { model | money = newValue }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                ( newDice, newSeed ) =
                    Random.step Dice.roll model.seed

                newModel =
                    model
                        |> setDice newDice
                        |> setSeed newSeed
            in
            ( newModel, Cmd.none )

        HandleAnimationFrameDelta delta ->
            let
                newMoney =
                    case Dice.numberWasRolled delta model.dice of
                        Just Dice.Two ->
                            model.money + 1

                        _ ->
                            model.money

                newModel =
                    model
                        |> updateDice (Dice.handleAnimationFrameDelta delta)
                        |> setMoney newMoney
            in
            ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen p-6 bg-gray-900 text-gray-100 flex space-x-4" ]
        [ div [ class "w-64 h-72 flex-col justify-center items-center space-y-4" ]
            [ div [ class "flex justify-center w-full" ]
                [ button
                    [ class "bg-blue-800 hover:bg-blue-700 active:bg-blue-600 cursor-pointer rounded shadow py-1 px-4"
                    , onClick Roll
                    ]
                    [ text "Roll" ]
                ]
            , Dice.render model.dice
            ]
        , div [ class "flex-grow h-72 flex justify-start items-center space-x-4" ]
            [ div [ class "h-full w-24 flex justify-center items-center" ] [ span [ class "text-6xl" ] [ text <| "$" ++ String.fromInt model.money ] ] ]
        ]
