module Main exposing (main)

import Basics.Extra
import Browser
import Browser.Events
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- DiceRollAnimation


bezierSlideFn : Easing
bezierSlideFn =
    Ease.bezier 0.02 0.01 1 -0.53


bezierSlideFn2 : Easing
bezierSlideFn2 =
    Ease.bezier 0.2 0.1 1 -0.61


bezierSlideFn3 : Easing
bezierSlideFn3 =
    Ease.bezier 0.02 0.01 0.93 -0.53


spins : number
spins =
    20


bezierSpinFn : Easing
bezierSpinFn =
    Ease.bezier 0 1.01 0 1.0


bezierSpinFn2 : Easing
bezierSpinFn2 =
    Ease.bezier 0.34 0.67 0.64 1


getTopDuringSpin : Float -> Float
getTopDuringSpin percentDone =
    let
        x =
            144

        n =
            spins

        top =
            -1 * x

        numerator =
            x * (2 * n + 1)

        denominator =
            1

        totalShiftDown =
            percentDone * (numerator / denominator)

        shiftDown =
            Basics.Extra.fractionalModBy (2 * x) totalShiftDown
    in
    top + shiftDown


getIthElement : Float -> Int
getIthElement percentDone =
    let
        n =
            spins
    in
    truncate (percentDone * (2 * n + 1) / 2)


type DiceRoll
    = SlideUp Float Float
    | SpinAnimation Float Float
    | Finished



-- MODEL


type Model
    = WaitingOnUser
    | Rolling DiceRoll


init : () -> ( Model, Cmd Msg )
init _ =
    ( WaitingOnUser, Cmd.none )



-- UPDATE


type Msg
    = HandleAnimationFrameDelta Float
    | Roll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        Roll ->
            ( Rolling (SlideUp 1000 0), Cmd.none )

        HandleAnimationFrameDelta delta ->
            case model of
                Rolling (SlideUp length time) ->
                    if time >= length then
                        ( Rolling (SpinAnimation 3000 0), Cmd.none )

                    else
                        ( Rolling (SlideUp length (time + delta)), Cmd.none )

                Rolling (SpinAnimation length time) ->
                    if time >= length then
                        ( Rolling Finished, Cmd.none )

                    else
                        ( Rolling (SpinAnimation length (time + delta)), Cmd.none )

                _ ->
                    noOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta



-- VIEW


renderDiceArea : Model -> Html Msg
renderDiceArea model =
    let
        renderDie : String -> Html Msg
        renderDie imageUrl =
            img [ class "w-8 h-8", src imageUrl ] []
    in
    div [ class "w-64 h-64 bg-gray-700 rounded-lg border-black overflow-hidden" ]
        [ div [ class "w-full h-full flex flex-col justify-evenly" ]
            (case model of
                Rolling (SlideUp duration time) ->
                    let
                        renderDieContainer : String -> Easing -> Float -> Html Msg
                        renderDieContainer imageUrl easingFn distance =
                            let
                                percentDone =
                                    time / duration

                                visualPercentDone =
                                    easingFn percentDone

                                topPx =
                                    -1 * visualPercentDone * distance

                                topPxStyle =
                                    String.fromFloat topPx ++ "px"
                            in
                            div [ class "relative w-8 h-8" ] [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie imageUrl ] ]
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ renderDieContainer "Dice-1-b.svg" bezierSlideFn 500
                        , renderDieContainer "Dice-2-b.svg" bezierSlideFn2 500
                        , renderDieContainer "Dice-3-b.svg" bezierSlideFn3 500
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDieContainer "Dice-4-b.svg" bezierSlideFn3 400
                        , renderDieContainer "Dice-5-b.svg" bezierSlideFn 400
                        , renderDieContainer "Dice-6a-b.svg" bezierSlideFn2 400
                        ]
                    ]

                Rolling (SpinAnimation duration time) ->
                    let
                        percentDone =
                            time / duration

                        visualPercentDone =
                            bezierSpinFn2 percentDone

                        topPx =
                            getTopDuringSpin visualPercentDone

                        topPxStyle =
                            String.fromFloat topPx ++ "px"

                        i =
                            getIthElement visualPercentDone

                        imageUrl =
                            case modBy 5 i of
                                0 ->
                                    "Dice-1-b.svg"

                                1 ->
                                    "Dice-2-b.svg"

                                2 ->
                                    "Dice-3-b.svg"

                                3 ->
                                    "Dice-4-b.svg"

                                4 ->
                                    "Dice-5-b.svg"

                                _ ->
                                    "Dice-6a-b.svg"
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie imageUrl ]
                            ]
                        ]
                    ]

                Rolling Finished ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ renderDie "Dice-1-b.svg"
                            ]
                        ]
                    ]

                WaitingOnUser ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie "Dice-1-b.svg"
                        , renderDie "Dice-2-b.svg"
                        , renderDie "Dice-3-b.svg"
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie "Dice-4-b.svg"
                        , renderDie "Dice-5-b.svg"
                        , renderDie "Dice-6a-b.svg"
                        ]
                    ]
            )
        ]


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen p-6 bg-gray-900 text-gray-100 flex space-x-4" ]
        [ div [ class "w-64 flex-col justify-center items-center space-y-4" ]
            [ div [ class "flex justify-center w-full" ]
                [ button
                    [ class "bg-blue-800 hover:bg-blue-700 active:bg-blue-600 cursor-pointer rounded shadow py-1 px-4"
                    , onClick Roll
                    ]
                    [ text "Roll" ]
                ]
            , renderDiceArea model
            ]
        ]
