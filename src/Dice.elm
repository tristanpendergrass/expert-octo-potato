module Dice exposing (Dice(..), create, handleAnimationFrameDelta, render, roll)

import Basics.Extra
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random


type Dice
    = WaitingOnUser
    | SlideUp Float Float
    | SpinAnimation Float Float
    | Finished


create : Dice
create =
    WaitingOnUser


roll : Random.Generator Dice
roll =
    Random.constant (SlideUp 1000 0)


handleAnimationFrameDelta : Float -> Dice -> Dice
handleAnimationFrameDelta delta dice =
    case dice of
        SlideUp duration time ->
            if time >= duration then
                SpinAnimation 3000 0

            else
                SlideUp duration (time + delta)

        SpinAnimation length time ->
            if time >= length then
                Finished

            else
                SpinAnimation length (time + delta)

        _ ->
            dice



-- VIEW


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


dieOne : String
dieOne =
    "Dice-1-b.svg"


dieTwo : String
dieTwo =
    "Dice-2-b.svg"


dieThree : String
dieThree =
    "Dice-3-b.svg"


dieFour : String
dieFour =
    "Dice-4-b.svg"


dieFive : String
dieFive =
    "Dice-5-b.svg"


dieSix : String
dieSix =
    "Dice-6a-b.svg"


render : Dice -> Html msg
render dice =
    let
        renderDie : String -> Html msg
        renderDie imageUrl =
            img [ class "w-8 h-8", src imageUrl ] []
    in
    div [ class "w-64 h-64 bg-gray-700 rounded-lg border-black overflow-hidden" ]
        [ div [ class "w-full h-full flex flex-col justify-evenly" ]
            (case dice of
                WaitingOnUser ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie dieOne
                        , renderDie dieTwo
                        , renderDie dieThree
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie dieFour
                        , renderDie dieFive
                        , renderDie dieSix
                        ]
                    ]

                SlideUp duration time ->
                    let
                        renderDieContainer : String -> Easing -> Float -> Html msg
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
                        [ renderDieContainer dieOne bezierSlideFn 500
                        , renderDieContainer dieTwo bezierSlideFn2 500
                        , renderDieContainer dieThree bezierSlideFn3 500
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDieContainer dieFour bezierSlideFn3 400
                        , renderDieContainer dieFive bezierSlideFn 400
                        , renderDieContainer dieSix bezierSlideFn2 400
                        ]
                    ]

                SpinAnimation duration time ->
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
                                    dieOne

                                1 ->
                                    dieTwo

                                2 ->
                                    dieThree

                                3 ->
                                    dieFour

                                4 ->
                                    dieFive

                                _ ->
                                    dieSix
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie imageUrl ]
                            ]
                        ]
                    ]

                Finished ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ renderDie "Dice-1-b.svg"
                            ]
                        ]
                    ]
            )
        ]
