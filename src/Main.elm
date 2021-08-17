module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "Why hello there", Cmd.none )



-- UPDATE


type Msg
    = HandleThingInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        HandleThingInput str ->
            ( str, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


renderDiceArea : Html Msg
renderDiceArea =
    div [ class "w-64 h-64 bg-gray-700 rounded-lg border-black" ]
        [ div [ class "w-full h-full flex flex-col justify-evenly" ]
            [ div [ class "flex w-full justify-evenly" ]
                [ img [ class "w-8 h-8", src "Dice-1-b.svg" ] []
                , img [ class "w-8 h-8", src "Dice-2-b.svg" ] []
                , img [ class "w-8 h-8", src "Dice-3-b.svg" ] []
                ]
            , div [ class "flex w-full justify-evenly" ]
                [ img [ class "w-8 h-8", src "Dice-4-b.svg" ] []
                , img [ class "w-8 h-8", src "Dice-5-b.svg" ] []
                , img [ class "w-8 h-8", src "Dice-6a-b.svg" ] []
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen p-6 bg-gray-900 text-gray-100 flex space-x-4" ]
        [ div [ class "w-64 flex-col justify-center items-center space-y-4" ]
            [ div [ class "flex justify-center w-full" ]
                [ button [ class "bg-blue-800 hover:bg-blue-700 active:bg-blue-600 cursor-pointer rounded shadow py-1 px-4" ] [ text "Roll" ]
                ]
            , renderDiceArea
            ]
        ]
