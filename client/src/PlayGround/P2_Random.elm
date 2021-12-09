module PlayGround.P2_Random exposing (..)

--import PlayGround.P2_Random exposing (..)
--import Random exposing Random
--https://package.elm-lang.org/packages/elm/random/latest/Random#Seed
--elm-community/random-extra great package for generating random values easier

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random

--testing out function
photoList = [{url="1.jpeg"}
            ,{url="2.jpeg"}
            ,{url="3.jpeg"}]
photoArray = Array.fromList photoList

randomPhotoPicker : Random.Generator Int 
randomPhotoPicker = 
    Random.int 0 (Array.length photoArray - 1)

randomPhotoPicker2 : Random.Generator Int 
randomPhotoPicker2 = 
    Random.int 0 1000


--Random number between 0 and 2
randomInt = Random.int 0 2

randomGen  : (a -> msg) -> Random.Generator a -> Cmd msg
randomGen = Random.generate



-- Small random app
type Msg = 
    ClickedRandomButton1
    | ClickedRandomButton2
    | GetRandomNumber Int

type alias Model = {randomNumber: Int}


view : Model -> Html Msg 
view model = 
    div [ class "content"] 
        [ h1 [] [ text "Random Number"]
        , label [] [ text (String.fromInt model.randomNumber)]
        , button [onClick (ClickedRandomButton1)] [ text "Random Number"]
        , button [onClick (ClickedRandomButton2)] [ text "Random Number 2"]
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
        case msg of 
            ClickedRandomButton1 -> 
                (model, Random.generate GetRandomNumber randomPhotoPicker)
            ClickedRandomButton2 -> 
                (model, Random.generate GetRandomNumber randomPhotoPicker2)
            GetRandomNumber index -> 
                ( {model| randomNumber = index}, Cmd.none)
            

initialModel : Model
initialModel = {randomNumber = -1}

main: Program () Model Msg
main = 
    Browser.element
    { init = \flags -> (initialModel, Cmd.none)
    , view = view 
    , update = update
    , subscriptions = \model -> Sub.none}


--**************************** Mapping for Random values *******************************************************
myMap : (a -> b) -> Random.Generator a -> Random.Generator b 
myMap = Random.map

bool : Random.Generator Bool 
bool = 
    Random.map (\n -> n < 20) (Random.int 1 100)

type alias Enemy =
    { health : Float 
    , rotation : Float 
    , x : Float 
    , y : Float 
    }
myMap2: (a -> b -> c) -> Random.Generator a -> Random.Generator b -> Random.Generator c
myMap2 = Random.map2

enemy : Random.Generator Enemy 
enemy = 
    Random.map2 
        (\x y -> Enemy 100 0 x y)
        (Random.float 0 100)
        (Random.float 0 100)

--manual enemy
myEnemy : (Enemy, Random.Seed)
myEnemy = Random.step enemy seed_0



--weighted
type Face = One | Two | Three | Four | Five | Six 

roll : Random.Generator Face 
roll = 
    Random.weighted 
        (10, One) -- a little strange with this part
        [ ( 10, Two)
        , ( 10, Three)
        , ( 10, Four)
        , ( 20, Five)
        , ( 40, Six)]

testOutRoll : (Face, Random.Seed)
testOutRoll = Random.step roll seed_0

getFirst : (a,b) -> a
getFirst (a, b) = a
testOutRoll2 : Face 
testOutRoll2 = getFirst (Random.step roll seed_0)


---**** and then *******
--andThen : (a -> Generator b) -> Generator a -> Generator b
type Color = 
    Red 
    | Blue 
    | Yellow 
    | Green 
    | Purple 
    | Orange 
    | Indigo 

colorToNumber : Color -> Random.Generator Int
colorToNumber color = 
    case color of 
        Red -> Random.int 0 10 
        Blue -> Random.int 0 100 
        Yellow -> Random.int 0 50 
        Green -> Random.int 0 15 
        Purple -> Random.int 0 3
        Orange -> Random.int 0 55 
        Indigo -> Random.int 0 11

colorGenerator: Random.Generator Color 
colorGenerator = 
        Random.weighted
            (100, Green)
            [(101, Yellow)
            ,(74,Orange )
            ,(50, Blue)
            ,(68, Indigo)
            , (140, Purple)
            , (30, Red)]

colorAndThen : Random.Generator Int
colorAndThen = 
    Random.andThen colorToNumber colorGenerator

tryColor = Random.step colorAndThen seed_0

--Let's try the other way around
numberToColor : Int -> Random.Generator Color 
numberToColor num = 
    case num of 
        1 -> weightedColor Blue 
        2 -> weightedColor Yellow
        3 -> weightedColor Orange 
        4 -> weightedColor Purple 
        5 -> weightedColor Indigo 
        6 -> weightedColor Red 
        7 -> weightedColor Green 
        _ -> weightedColor Green

weightedColor color = 
    Random.weighted 
        (100, color)
        [(1, Yellow)
        ,(1, Green)
        ,(1, Red)
        ,(1, Purple)
        ,(1, Blue)
        ,(1, Orange)
        ,(1, Indigo)]

numberColorAndThen : Random.Generator Color
numberColorAndThen = Random.andThen numberToColor (Random.int 0 8)

tryNumberColorAndThen = Random.step numberColorAndThen

--coolMap : (a-> b) -> Random.Generator a -> 1

pureLikeFunction : Random.Generator Color 
pureLikeFunction = Random.constant Orange

--**************************** Generate Values Manually ******************************************************

--random step type annotation
myRandomStep : Random.Generator a -> Random.Seed -> (a, Random.Seed)
myRandomStep = Random.step

myRandomInitial : Int -> Random.Seed 
myRandomInitial = Random.initialSeed
--
type alias Point3D = { x: Float, y: Float, z : Float}

point3D : Random.Seed -> (Point3D, Random.Seed)
point3D seed0 =
    let
        (x, seed1) = Random.step (Random.float 0 100) seed0
        (y, seed2) = Random.step (Random.float 0 100) seed1 
        (z, seed3) = Random.step (Random.float 0 100) seed2
    in
        (Point3D x y z, seed3)


--create initial seed
seed_0 : Random.Seed 
seed_0 = 
    Random.initialSeed 42

yourOwnSeed : Int -> Random.Seed 
yourOwnSeed num = 
    Random.initialSeed num

--choose a random color 
randomColor : Random.Generator Color
randomColor = Random.uniform Orange [Red, Blue, Indigo, Purple, Yellow, Green]

myRandomColor = Random.step randomColor seed_0