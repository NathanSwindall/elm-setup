module PlayGround.P1_Arrays exposing (..)

import Array exposing (Array)
--import PlayGround.P1_Arrays exposing (..)

myList = [1,2,3,4,5,6]
myArray = Array.fromList myList 
myValue = Array.get 2 myArray

getPhotoUrl: Int -> String 
getPhotoUrl index = 
    case Array.get index photoArray of 
        Just photo -> 
            photo.url 
        Nothing -> 
            ""

photoList = 
    [{url="1.jpeg"}
    ,{url="2.jpeg"}
    ,{url="3.jpeg"}]

photoArray = Array.fromList photoList