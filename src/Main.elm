module Main exposing (main)

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


type DiceRoll
    = SlideUp Float Float
    | SpinAnimation
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
                        ( Rolling SpinAnimation, Cmd.none )

                    else
                        ( Rolling (SlideUp length (time + delta)), Cmd.none )

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
        renderDie : String -> Easing -> Float -> Html Msg
        renderDie imageUrl easingFn distance =
            case model of
                Rolling (SlideUp duration time) ->
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
                    img [ class "w-8 h-8", src imageUrl, class "absolute left-0 right-0", style "top" (Debug.log "topPx" topPxStyle) ] []

                _ ->
                    img [ class "w-8 h-8", src imageUrl ] []
    in
    div [ class "w-64 h-64 bg-gray-700 rounded-lg border-black overflow-hidden" ]
        [ div [ class "w-full h-full flex flex-col justify-evenly" ]
            [ div [ class "flex w-full justify-evenly relative" ]
                [ div [ class "relative w-8 h-8" ] [ renderDie "Dice-1-b.svg" bezierSlideFn 500 ]
                , div [ class "relative w-8 h-8" ] [ renderDie "Dice-2-b.svg" bezierSlideFn2 500 ]
                , div [ class "relative w-8 h-8" ] [ renderDie "Dice-3-b.svg" bezierSlideFn3 500 ]
                ]
            , div [ class "flex w-full justify-evenly relative" ]
                [ div [ class "relative w-8 h-8" ] [ renderDie "Dice-4-b.svg" bezierSlideFn 300 ]
                , div [ class "relative w-8 h-8" ] [ renderDie "Dice-5-b.svg" bezierSlideFn2 300 ]
                , div [ class "relative w-8 h-8" ] [ renderDie "Dice-6a-b.svg" bezierSlideFn3 300 ]
                ]
            ]
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
