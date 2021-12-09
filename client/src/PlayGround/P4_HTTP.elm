module PlayGround.P4_HTTP exposing (..)

import Http exposing (get)
--import PlayGround.P4_HTTP exposing (..)
--Http.get {url : String, expect : Expect msg} -> Cmd msg

-- manningWebsite = get 
--                     { url = "htpp://manning.com"
--                     , expect = Http.expectString toMsg}



myHTTPGet : {url : String, expect: Http.Expect msg} -> Cmd msg 
myHTTPGet = get

myExpectString : (Result Http.Error String -> msg) -> Http.Expect msg 
myExpectString = Http.expectString


type MyResult errValue okValue 
    = Err errValue 
    | Ok okValue

type MyError 
    = BadUrl String 
    | Timeout 
    | NetworkError
    | BadStatus Int 
    | BadBody String 