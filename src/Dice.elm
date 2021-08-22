module Dice exposing (Dice(..), create, handleAnimationFrameDelta, render, roll)

import Basics.Extra
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import List.Nonempty exposing (Nonempty(..))
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


dieFaces : Nonempty DieFace
dieFaces =
    Nonempty One [ Two, Three, Four, Five, Six ]


type Dice
    = WaitingOnUser
    | SlideUp Float DieFace (List DieFace)
    | SpinAnimation Float DieFace (List DieFace)
    | Finished DieFace


create : Dice
create =
    WaitingOnUser


dieFaceExcluding : DieFace -> Random.Generator DieFace
dieFaceExcluding dieFace =
    let
        ( head, list ) =
            case dieFace of
                One ->
                    ( Two, [ Three, Four, Five, Six ] )

                Two ->
                    ( One, [ Three, Four, Five, Six ] )

                Three ->
                    ( Two, [ One, Four, Five, Six ] )

                Four ->
                    ( Two, [ One, Three, Five, Six ] )

                Five ->
                    ( Two, [ One, Three, Four, Six ] )

                Six ->
                    ( Two, [ One, Three, Four, Five ] )
    in
    Random.uniform head list


dieFaceSequence : Int -> Random.Generator (Nonempty DieFace)
dieFaceSequence size =
    if size <= 1 then
        List.Nonempty.sample dieFaces
            |> Random.map List.Nonempty.singleton

    else
        dieFaceSequence (size - 1)
            |> Random.andThen
                (\(Nonempty head tail) ->
                    Random.map (\item -> Nonempty item (head :: tail)) (dieFaceExcluding head)
                )


roll : Random.Generator Dice
roll =
    dieFaceSequence spins
        |> Random.map (\(Nonempty result sequence) -> SlideUp 0 result sequence)


handleAnimationFrameDelta : Float -> Dice -> Dice
handleAnimationFrameDelta delta dice =
    case dice of
        WaitingOnUser ->
            dice

        SlideUp time result faceSequence ->
            if time >= slideDuration then
                SpinAnimation 0 result faceSequence

            else
                SlideUp (time + delta) result faceSequence

        SpinAnimation time result faceSequence ->
            if time >= spinDuration then
                Finished result

            else
                SpinAnimation (time + delta) result faceSequence

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

                SlideUp time result faceSequence ->
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

                SpinAnimation time result faceSequence ->
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
                            List.Extra.getAt i (Debug.log "faceSequence" faceSequence)
                                |> Maybe.withDefault One
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
