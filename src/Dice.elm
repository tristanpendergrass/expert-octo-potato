module Dice exposing (Dice(..), create, handleAnimationFrameDelta, render, roll)

import Basics.Extra
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Nonempty exposing (Nonempty(..))
import Random


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
    | SlideUp Float (Nonempty DieFace)
    | SpinAnimation Float (Nonempty DieFace)
    | Finished DieFace


numSpins : number
numSpins =
    15


slideDuration : number
slideDuration =
    1000


spinDuration : number
spinDuration =
    3000


create : Dice
create =
    WaitingOnUser


roll : Random.Generator Dice
roll =
    dieFaceSequence (numSpins + 1)
        |> Random.map (SlideUp 0)


dieFacesWithout : DieFace -> Nonempty DieFace
dieFacesWithout dieFace =
    case dieFace of
        One ->
            Nonempty Two [ Three, Four, Five, Six ]

        Two ->
            Nonempty One [ Three, Four, Five, Six ]

        Three ->
            Nonempty One [ Two, Four, Five, Six ]

        Four ->
            Nonempty One [ Two, Three, Five, Six ]

        Five ->
            Nonempty One [ Two, Three, Four, Six ]

        Six ->
            Nonempty One [ Two, Three, Four, Five ]


dieFaceSequence : Int -> Random.Generator (Nonempty DieFace)
dieFaceSequence size =
    if size <= 1 then
        List.Nonempty.sample dieFaces
            |> Random.map List.Nonempty.singleton

    else
        dieFaceSequence (size - 1)
            |> Random.andThen
                (\(Nonempty head tail) ->
                    dieFacesWithout head
                        |> List.Nonempty.sample
                        |> Random.map (\item -> Nonempty item (head :: tail))
                )


handleAnimationFrameDelta : Float -> Dice -> Dice
handleAnimationFrameDelta delta dice =
    case dice of
        WaitingOnUser ->
            dice

        SlideUp time faceSequence ->
            if time >= slideDuration then
                SpinAnimation 0 faceSequence

            else
                SlideUp (time + delta) faceSequence

        SpinAnimation time faceSequence ->
            if time >= spinDuration then
                Finished (List.Nonempty.last faceSequence)

            else
                SpinAnimation (time + delta) faceSequence

        Finished _ ->
            dice



-- VIEW


dieDimensions : { small : String, large : String }
dieDimensions =
    { small = "w-8 h-8", large = "w-12 h-12" }



-- Distance between each die face in spinner


spinDistance : number
spinDistance =
    -- 144
    200


bezierSlideFn : Easing
bezierSlideFn =
    Ease.bezier 0.02 0.01 1 -0.53


bezierSlideFn2 : Easing
bezierSlideFn2 =
    Ease.bezier 0.2 0.1 1 -0.61


bezierSlideFn3 : Easing
bezierSlideFn3 =
    Ease.bezier 0.02 0.01 0.93 -0.53


bezierSpinFn : Easing
bezierSpinFn =
    Ease.bezier 0.34 0.67 0.64 1


topPxAtPercentDone : Float -> Float
topPxAtPercentDone percentDone =
    let
        top =
            -1 * spinDistance

        totalLength : number
        totalLength =
            2 * spinDistance * numSpins + spinDistance

        lengthShiftedDown =
            percentDone * totalLength

        shiftDown =
            Basics.Extra.fractionalModBy (2 * spinDistance) lengthShiftedDown
    in
    top + shiftDown


ithElementAtPercentDone : Float -> Int
ithElementAtPercentDone percentDone =
    truncate (percentDone * (2 * numSpins + 1) / 2)


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
            img [ class dieDimensions.small, src <| getUrl dieFace ] []

        renderDieLarge : DieFace -> Html msg
        renderDieLarge dieFace =
            img [ class dieDimensions.large, src <| getUrl dieFace ] []
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

                SlideUp time _ ->
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
                            div [ class "relative", class dieDimensions.small ] [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDie dieFace ] ]
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

                SpinAnimation time faceSequence ->
                    let
                        percentDone =
                            time / spinDuration

                        visualPercentDone =
                            bezierSpinFn percentDone

                        topPx =
                            topPxAtPercentDone visualPercentDone

                        topPxStyle =
                            String.fromFloat topPx ++ "px"

                        i =
                            ithElementAtPercentDone visualPercentDone

                        dieFace =
                            List.Nonempty.get i faceSequence
                    in
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative", class dieDimensions.large ]
                            [ div [ class "absolute left-0 right-0", style "top" topPxStyle ] [ renderDieLarge dieFace ]
                            ]
                        ]
                    ]

                Finished result ->
                    [ div [ class "flex w-full justify-evenly relative" ]
                        [ div [ class "relative", class dieDimensions.large ]
                            [ renderDieLarge result
                            ]
                        ]
                    ]
            )
        ]
