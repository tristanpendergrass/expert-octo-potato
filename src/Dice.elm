module Dice exposing (Dice(..), create, handleAnimationFrameDelta, render, roll)

import Basics.Extra
import Ease exposing (Easing)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra
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



-- TODO: Refactor to use non-empty list as return type (and argument?)


dieFaceSequence : Int -> DieFace -> List DieFace -> Random.Generator (List DieFace)
dieFaceSequence size endResult candidates =
    if size == 0 then
        Random.constant []

    else if size == 1 then
        -- The last die face in the sequence should not match a certain die face (which is the end result of the larger sequence)
        dieFaceExcluding endResult
            |> Random.map List.singleton

    else
        -- Each other die face in the sequence should not match the previous one to look more random when spinning
        dieFaceSequence (size - 1) endResult candidates
            |> Random.andThen
                (\list ->
                    case list of
                        nextItem :: _ ->
                            let
                                currentItem =
                                    dieFaceExcluding nextItem
                            in
                            Random.map (\item -> item :: list) currentItem

                        _ ->
                            -- not reachable
                            Random.constant []
                )


pseudoRandomUniform : Int -> DieFace -> List DieFace -> Random.Generator ( DieFace, List DieFace )
pseudoRandomUniform size default list =
    Random.uniform default list
        |> Random.andThen
            (\result ->
                dieFaceSequence size result list
                    |> Random.map (\resultList -> ( result, resultList ))
            )


roll : Random.Generator Dice
roll =
    pseudoRandomUniform spins One [ One, Two, Three, Four, Five, Six ]
        |> Random.map (\( face, sequence ) -> SlideUp 0 face sequence)


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
