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


type alias Buildings =
    { meadows : Int
    , smiths : Int
    }


type Phase
    = RollPhase Dice
    | BuildPhase Building Building


type alias Model =
    { seed : Random.Seed
    , phase : Phase
    , money : Int
    , buildings : Buildings
    , roundPanelIsOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 0
      , phase = RollPhase Dice.create
      , money = 0
      , buildings = { meadows = 1, smiths = 2 }
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


view : Model -> Html Msg
view model =
    sidebar [ class "h-full ", attribute "sideWidth" "35%" ]
        [ cover [ attribute "centered" ".roll-container", class "border-r-4 border-gray-100 border-dotted" ]
            [ stack [ class "roll-container" ]
                (case model.phase of
                    RollPhase dice ->
                        [ center []
                            [ button
                                [ class "bg-blue-800 hover:bg-blue-700 active:bg-blue-600 cursor-pointer rounded shadow py-1 px-4"
                                , onClick Roll
                                ]
                                [ text "Roll" ]
                            ]
                        , center [] [ Dice.render dice ]
                        ]

                    _ ->
                        []
                )
            ]
        , cover [ attribute "centered" ".buildings-container" ]
            [ center []
                [ h2 [ class "text-6xl" ] [ text "$0" ]
                ]
            , center [ class "buildings-container" ]
                [ renderBuildings model
                ]
            ]
        ]
