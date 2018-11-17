import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Model =
  { initNutHeightTreb : String
  , initNutHeightBass : String
  , initBridgeHeight : String
  , scanLastFret : String
  , goalBridgeHeight : String
  , goalFallOff : String
  , cut1stFret : String
  , cut12thFret : String
  }


init : Model
init =
  Model
    "7" "7" "9"
    "0"
    "9" "0.2"
    "" ""


-- UPDATE


type Msg
  = InitNutHeightTreb String
  | InitNutHeightBass String
  | InitBridgeHeight String
  | ScanLastFret String
  | GoalBridgeHeight String
  | GoalFallOff String
  | Cut1stFret String
  | Cut12thFret String


update : Msg -> Model -> Model
update msg model =
  case msg of
    InitNutHeightTreb initNutHeightTreb ->
      { model | initNutHeightTreb = initNutHeightTreb }

    InitNutHeightBass initNutHeightBass ->
      { model | initNutHeightBass = initNutHeightBass }

    InitBridgeHeight initBridgeHeight ->
      { model | initBridgeHeight = initBridgeHeight }
    
    ScanLastFret scanLastFret -> 
      { model | scanLastFret = scanLastFret }

    GoalBridgeHeight goalBridgeHeight ->
      { model | goalBridgeHeight = goalBridgeHeight }

    GoalFallOff goalFallOff ->
      { model | goalFallOff = goalFallOff }

    Cut1stFret cut1stFret ->
      { model | cut1stFret = cut1stFret
      , cut12thFret =
        maybeCut12thCalc  model.initBridgeHeight model.goalBridgeHeight cut1stFret  }

    Cut12thFret cut12thFret ->
      { model | cut12thFret = cut12thFret
      , cut1stFret =
        maybeCut1stCalc  model.initBridgeHeight model.goalBridgeHeight cut12thFret  }

maybeCut1stCalc initBH goalBH cut12th =
  case Maybe.map3 cut1stCalc (String.toFloat initBH) (String.toFloat goalBH) (String.toFloat cut12th)  of
    Just result ->
      String.fromFloat result
    Nothing ->
      "Error"

cut1stCalc initBH goalBH cut12th =
  ( ( goalBH - initBH ) + 2 * cut12th / 1000) * 1000

maybeCut12thCalc initBH goalBH cut1st =
  case Maybe.map3 cut12thCalc (String.toFloat initBH) (String.toFloat goalBH) (String.toFloat cut1st)  of
    Just result ->
      String.fromFloat result
    Nothing ->
      "Error"

cut12thCalc initBH goalBH cut1st =
  ( cut1st / 1000 + ( goalBH - initBH ) ) / 2 * 1000

-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ header [ class "page-header"]
    [ h1 [] [ text "Fingerboard Surfacing Calculator" ]
    ]
  , h2 [] [ text "Initial Measurements"]
  , table [ class "table" ]
    [ tr []
      [ th [] [ text "FB Bridge Height (mm)" ]
      , td []
        [ input
          [ class "form-control"
          , value model.initBridgeHeight
          , onInput InitBridgeHeight
          ]
          []
        ]
      ]
    ]
  , h2 [] [ text "Goal Values"]
  , table [ class "table" ]
    [ tr []
      [ th [] [ text "FB Bridge Height (mm)" ]
      , td []
        [ input
          [ class "form-control"
          , value model.goalBridgeHeight
          , onInput GoalBridgeHeight
          ] []
        ]
      ]
    ]
  , h2 [] [ text "Machine Parameters (at centre)" ]
  , table [ class "table" ]
    [ tr []
      [ th [] [ text "Cut at 1st Fret (μm)" ]
      , td []
        [ input
          [ class "form-control"
          , value model.cut1stFret
          , onInput Cut1stFret
          ] []
        ] 
      ]
    , tr []
      [ th [] [ text "Cut at 12th Fret (μm)"]
      , td []
        [ input
          [ class "form-control"
            , value model.cut12thFret
            , onInput Cut12thFret
          ] []
        ]
      ]
    ]
  ]

