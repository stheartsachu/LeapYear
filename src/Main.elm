module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, input, li, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)



-- MAIN


main : Program () Model Msg
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


renderlist : List Int -> Html msg
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


yearValidation : String -> ( String, Bool )
yearValidation stringArg =
    if String.length stringArg > 3 && String.length stringArg == 4 then
        ( "valid ", True )

    else
        ( "In-valid", False )


view : Model -> Html Msg
view model =
    div []
        [ span [] [ text "Assignment : Elm application with one single page, having two input types for inserting Years(Validation should be implemented). You have to print all the Leap Year between these two entered years. Entered years should not be included in the results." ]
        , div []
            [ span [] [ text "From :::: YearOne : " ]
            , input [ onInput InputedYearOne ] []
            ]
        , div []
            [ span [] [ text "TO ::::    YearTwo : " ]
            , input [ onInput InputedYearTwo ] []
            ]
        , if String.length model.yearOne == 0 && String.length model.yearTwo == 0 then
            span [] [ text "Fistly Input Years to Check" ]

          else
            div []
                [ div []
                    [ p [] [ text ("Year One  : " ++ Tuple.first (yearValidation model.yearOne)) ]
                    , p [] [ text ("Year Two  :  " ++ Tuple.first (yearValidation model.yearTwo)) ]
                    ]
                , if Tuple.second (yearValidation model.yearOne) && Tuple.second (yearValidation model.yearTwo) then
                    if List.isEmpty (yearsList model.intYearOne model.intYearTwo) then
                        span [] [ text "First Input Year should be Greater than Second One" ]

                    else
                        div []
                            [ div [] [ text (toString model.intYearOne) ]
                            , div [] [ text (toString model.intYearTwo) ]
                            , div [] [ text ("List of Years between Two Entered Years : " ++ toString (yearsList model.intYearOne model.intYearTwo)) ]
                            , span [] [ text "List of Leap Years" ]
                            , renderlist (yearsList model.intYearOne model.intYearTwo)
                            ]

                  else
                    span [] [ text "Try to Input Valid Years" ]
                ]
        ]
