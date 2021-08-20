module Dice exposing (Dice(..), create, handleAnimationFrameDelta, render, roll)

import Basics.Extra
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random


slideDuration : number
slideDuration =
    1000


spinDuration : number
spinDuration =
    2000


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type Dice
    = WaitingOnUser
    | SlideUp DieFace (List DieFace) Float
    | SpinAnimation DieFace (List DieFace) Float
    | Finished DieFace


create : Dice
create =
    WaitingOnUser


roll : Random.Generator Dice
roll =
    Random.constant (SlideUp Six [ One, Two, Three ] 0)


handleAnimationFrameDelta : Float -> Dice -> Dice
handleAnimationFrameDelta delta dice =
    case dice of
        WaitingOnUser ->
            dice

        SlideUp result faceSequence time ->
            if time >= slideDuration then
                SpinAnimation result faceSequence 0

            else
                SlideUp result faceSequence (time + delta)

        SpinAnimation result faceSequence time ->
            if time >= spinDuration then
                Finished result

            else
                SpinAnimation result faceSequence (time + delta)

        Finished _ ->
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


topPxAtPercentDone : Float -> Float
topPxAtPercentDone percentDone =
    let
        x =
            144

        top =
            -1 * x

        totalLength : number
        totalLength =
            2 * x * spins + x

        lengthShiftedDown =
            percentDone * totalLength

        shiftDown =
            Basics.Extra.fractionalModBy (2 * x) lengthShiftedDown
    in
    top + shiftDown


ithElementAtPercentDone : Float -> Int
ithElementAtPercentDone percentDone =
    truncate (percentDone * (2 * spins + 1) / 2)


getUrl : DieFace -> String
getUrl face =
    case face of
        One ->
            "Dice-1-b.svg"

        Two ->
            "Dice-2-b.svg"

        Three ->
            "Dice-3-b.svg"

        Four ->
            "Dice-4-b.svg"

        Five ->
            "Dice-5-b.svg"

        Six ->
            "Dice-6a-b.svg"


render : Dice -> Html msg
render dice =
    let
        renderDie : DieFace -> Html msg
        renderDie dieFace =
            img [ class "w-8 h-8", src <| getUrl dieFace ] []
    in
    div [ class "w-64 h-64 bg-gray-700 rounded-lg border-black overflow-hidden" ]
        [ div [ class "w-full h-full flex flex-col justify-evenly" ]
            (case dice of
                WaitingOnUser ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie One
                        , renderDie Two
                        , renderDie Three
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDie Four
                        , renderDie Five
                        , renderDie Six
                        ]
                    ]

                SlideUp result faceSequence time ->
                    let
                        renderDieContainer : DieFace -> Easing -> Float -> Html msg
                        renderDieContainer dieFace easingFn distance =
                            let
                                percentDone =
                                    time / slideDuration

                                visualPercentDone =
                                    easingFn percentDone

                                topPx =
                                    -1 * visualPercentDone * distance

                                topPxStyle =
                                    String.fromFloat topPx ++ "px"
                            in
                            div [ class "relative w-8 h-8" ] [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie dieFace ] ]
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ renderDieContainer One bezierSlideFn 500
                        , renderDieContainer Two bezierSlideFn2 500
                        , renderDieContainer Three bezierSlideFn3 500
                        ]
                    , div [ class "flex w-full justify-evenly relative" ]
                        [ renderDieContainer Four bezierSlideFn3 400
                        , renderDieContainer Five bezierSlideFn 400
                        , renderDieContainer Six bezierSlideFn2 400
                        ]
                    ]

                SpinAnimation result faceSequence time ->
                    let
                        percentDone =
                            time / spinDuration

                        visualPercentDone =
                            bezierSpinFn2 percentDone

                        topPx =
                            topPxAtPercentDone visualPercentDone

                        topPxStyle =
                            String.fromFloat topPx ++ "px"

                        i =
                            ithElementAtPercentDone visualPercentDone

                        dieFace =
                            case modBy 6 i of
                                0 ->
                                    One

                                1 ->
                                    Two

                                2 ->
                                    Three

                                3 ->
                                    Four

                                4 ->
                                    Five

                                _ ->
                                    Six
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie dieFace ]
                            ]
                        ]
                    ]

                Finished result ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative w-8 h-8" ]
                            [ renderDie result
                            ]
                        ]
                    ]
            )
        ]
