module Main where

import StartApp exposing (App)
import Html exposing (..)
import Effects exposing (Effects, Never)

import Random exposing (Seed)
import Signal exposing (Signal, Address)
import Task exposing (Task)
import Time exposing (Time)


-- MODEL

type alias Model =
  { startTime : Maybe Time
  , passedSeconds : Int
  , nextSeed : Maybe Seed
  , readyCount : Int
  , shoesCount : Int
  , events : List String
  }

init : (Model, Effects Action)
init =
  (Model Nothing 0 Nothing 0 0 [ ], Effects.none)


-- UPDATE

type Action
  = Tick Time
  | Start
  | GetReady String
  | Ready String Int
  | Arm
  | GetShoesOn String
  | Countdown
  | ShoesOn String Int
  | Exit
  | AlarmOn


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Tick currentTime ->
      case model.startTime of
        Just startTime ->
          let
            seconds =
              currentTime - startTime
                |> Time.inSeconds
                |> round
          in
            ({model | passedSeconds <- seconds}, Effects.none)
        Nothing ->
          ( { model |
                startTime <- Just currentTime
            ,   nextSeed <- Just (Random.initialSeed (round currentTime))
            }
          , Effects.tick (always Start)
          )

    Start ->
      ( addEvent model "Let's go for a walk!"
      , Effects.batch
          [ Effects.tick (always (GetReady "Bob"))
          , Effects.tick (always (GetReady "Alice"))
          ]
      )

    GetReady name ->
      let
        random =
          randomBetween 60 90 model.nextSeed
      in
        ( addEventWithSeed
            model
            (name ++ " started getting ready")
            random.newSeed
        , delayedEffect (toFloat random.number) (Ready name random.number)
        )

    Ready name delay ->
      let
        newEffects =
          if model.readyCount == 1 then
            Effects.tick (always Arm)
          else
            Effects.none
      in
        ( { model |
              events <- model.events ++ [timedEvent name delay "getting ready"]
          ,   readyCount <- model.readyCount + 1
          }
        , newEffects
        )

    Arm ->
      ( addEvent model "Arming alarm."
      , Effects.batch
          [ Effects.tick (always (GetShoesOn "Bob"))
          , Effects.tick (always (GetShoesOn "Alice"))
          , Effects.tick (always (Countdown))
          ]
      )

    GetShoesOn name ->
      let
        random =
          randomBetween 35 45 model.nextSeed
      in
        ( addEventWithSeed
            model
            (name ++ " started putting on shoes")
            random.newSeed
        , delayedEffect (toFloat random.number) (ShoesOn name random.number)
        )

    ShoesOn name delay ->
      let
        newEffects =
          if model.shoesCount == 1 then
            Effects.tick (always Exit)
          else
            Effects.none
      in
        ( { model |
              events <-
                model.events ++ [timedEvent name delay "putting on shoes"]
          ,   shoesCount <- model.shoesCount + 1
          }
        , newEffects
        )

    Countdown ->
      ( addEvent model "Alarm is counting down."
      , delayedEffect 60 AlarmOn
      )

    Exit ->
      ( addEvent model "Exiting and locking the door."
      , Effects.none
      )

    AlarmOn ->
      ( addEvent model "Alarm is armed."
      , Effects.none
      )


timedEvent : String -> Int -> String -> String
timedEvent name delay event =
  name ++ " spent " ++ (toString delay) ++ " seconds " ++ event


addEvent : Model -> String -> Model
addEvent model event =
  {model | events <- model.events ++ [event]}


addEventWithSeed : Model -> String -> Seed -> Model
addEventWithSeed model event seed =
  { model |
      events <- model.events ++ [event]
  ,   nextSeed <- Just seed
  }


randomBetween : Int -> Int -> Maybe Seed -> {number : Int, newSeed : Seed}
randomBetween low high seed =
  let
    currentSeed =
      Maybe.withDefault (Random.initialSeed 0) seed
    random =
      Random.generate (Random.int low high) currentSeed
    number =
      fst random
    newSeed =
      snd random
  in
    {number = number, newSeed = newSeed}


delayedEffect : Float -> Action -> Effects Action
delayedEffect delay action =
  Effects.task
    ( Task.sleep (delay * 1000)
        `Task.andThen` (\_ -> Task.succeed action)
    )


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ ]
    [ p
        [ ]
        [text ("Seconds:  " ++ (toString model.passedSeconds))]
    , ol
        [ ]
        (List.map (\e -> li [ ] [text e]) model.events)
    ]


-- APP

seconds : Signal Action
seconds =
  Signal.map Tick (Time.every Time.second)


app : App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [seconds] }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
