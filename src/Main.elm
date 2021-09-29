module Main exposing (..)

import Browser exposing (element)
import Debug exposing (toString)
import Html exposing (Html, div, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { yearOne : String
    , yearTwo : String
    , intYearOne : Int
    , intYearTwo : Int
    }


init : Model
init =
    { yearOne = ""
    , yearTwo = ""
    , intYearOne = 0
    , intYearTwo = 0
    }



-- UPDATE


type Msg
    = InputedYearOne String
    | InputedYearTwo String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputedYearOne inputArgumentOne ->
            { model | yearOne = inputArgumentOne, intYearOne = validationForInput inputArgumentOne }

        InputedYearTwo inputArgumentTwo ->
            { model | yearTwo = inputArgumentTwo, intYearTwo = validationForInput inputArgumentTwo }


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && not (modBy 100 year == 0) || modBy 400 year == 0


yearsList : Int -> Int -> List Int
yearsList yearone yeartwo =
    List.range (yearone + 1) (yeartwo - 1)


validationForInput : String -> Int
validationForInput str =
    case String.toInt str of
        Just intvalue ->
            intvalue

        Nothing ->
            0


renderlist lst =
    ul []
        (List.map
            (\element ->
                if isLeapYear element then
                    li [] [ text (toString element) ]

                else
                    span [] []
            )
            lst
        )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "YearOne : " ]
            , input [ onInput InputedYearOne ] []
            ]
        , div []
            [ span [] [ text "YearTwo : " ]
            , input [ onInput InputedYearTwo ] []
            ]
        , div [] [ text (toString model.intYearOne) ]
        , div [] [ text (toString model.intYearTwo) ]
        , div [] [ text (toString (yearsList model.intYearOne model.intYearTwo)) ]
        , renderlist (yearsList model.intYearOne model.intYearTwo)
        ]
