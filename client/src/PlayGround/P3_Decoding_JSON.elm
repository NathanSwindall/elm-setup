module PlayGround.P3_Decoding_JSON exposing (..)


--import PlayGround.P3_Decoding_JSON exposing (..)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Decode exposing (Decoder)
import Xml.Decode



myResultType : String -> Result String String
myResultType str = 
    case str of 
        "Nathan" -> Result.Ok "This is the Correct Answer"
        _        -> Result.Err "You don't know the coolest name in the world"

myDecodeString: Json.Decode.Decoder val -> String -> Result Json.Decode.Error val 
myDecodeString = Json.Decode.decodeString



--Decoding booleans
myBool: Result Json.Decode.Error Bool 
myBool = Json.Decode.decodeString Json.Decode.bool "true"

--Decoding floats
myFloat : Result Json.Decode.Error Float 
myFloat = Json.Decode.decodeString Json.Decode.float "3.33389"

multiplyFloat: Result Json.Decode.Error Float -> Float 
multiplyFloat result = 
    case result of 
        Ok num -> 3.33 * num 
        Err _-> 3.33

--Decoding lists
myList : Result Json.Decode.Error (List String)
myList = Json.Decode.decodeString (Json.Decode.list (Json.Decode.string)) "[\"hello\"]"


--you can also use the three quotes
myListThreeQuotes = Json.Decode.decodeString (Json.Decode.list (Json.Decode.string)) """["hello"]"""

--Decoding field functions
objectDecoder : Json.Decode.Decoder String 
objectDecoder = 
    Json.Decode.field "email" Json.Decode.string 

objectDecoding : Result Json.Decode.Error String
objectDecoding = Json.Decode.decodeString objectDecoder """{"email": "cate@nolf.com"}"""

--multiple field
myDecodeMap : Json.Decode.Decoder (Int, Int)
myDecodeMap = Json.Decode.map2 
                (\x y -> (x,y))
                (Json.Decode.field "x" Json.Decode.int)
                (Json.Decode.field "y" Json.Decode.int)

myDecodeMapDecoder : Result Json.Decode.Error ( Int, Int )
myDecodeMapDecoder = Json.Decode.decodeString myDecodeMap """{"x": 5, "y":12}"""

--Photo: String -> Int -> String -> Photo
type alias Photo = 
    { url : String 
    , size : Int
    , title: String
    }


photoDecoder : Json.Decode.Decoder Photo 
photoDecoder = 
    Json.Decode.map3 
        (\url size title -> { url = url, size = size, title = title})
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "size" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)

--using the alias as a function
photoDecoder2 : Json.Decode.Decoder Photo 
photoDecoder2 = 
    Json.Decode.map3 
        (Photo)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "size" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)

photoDecoding : Result Json.Decode.Error Photo
photoDecoding = Json.Decode.decodeString photoDecoder2 """{"url": "1.jpeg", "size": 36, "title": "Beachside"}"""

--For larger scale json decoding use package elm install NoRedInk/elm-json-decode-pipeline
photoDecoder3 : Json.Decode.Decoder Photo 
photoDecoder3 = 
    Json.Decode.succeed Photo 
        |> required "url" Json.Decode.string 
        |> required "size" Json.Decode.int 
        |> optional "title" Json.Decode.string "(untitled)"


testJson: String 
testJson =   """{"url": "www.google.com", "name": "Nathan", "size": 1000, "title": "Google"}"""

photoDecoding2 : Result Json.Decode.Error Photo
photoDecoding2 = Json.Decode.decodeString photoDecoder3 testJson

myRequired: String -> Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Decoder b
myRequired = required

myOptional: String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
myOptional= optional

--This seems like a pure function
--It puts it into the decoder context
mySucceed: a -> Json.Decode.Decoder a 
mySucceed = Json.Decode.succeed

type alias Address = 
    { address: String
    , number: Int 
    , name: String
    , occupation: String
    }



{-
mySucceed Address : Decoder (String -> Int -> String -> String -> Address)
|> required "address" string 
            Decoder (String -> Int -> String -> String -> Address)
                a = String 
                b = Int -> String -> String -> Address
            Decoder (Int-> String -> String -> Address)

-}


--Xml

type alias Data = 
    { string : String 
    , integers : List Int}

dataDecoder : Xml.Decode.Decoder Data 
dataDecoder = 
    Xml.Decode.map2 Data 
        (Xml.Decode.path ["path", "to", "string", "value"] (Xml.Decode.single Xml.Decode.string))
        (Xml.Decode.path ["path", "to", "int", "values"] (Xml.Decode.list Xml.Decode.int))


xmlTest =
    """
    <root>
        <path>
            <to>
                <string>
                    <value>SomeString</value>
                </string>
                <int>
                    <values>1</values>
                    <values>2</values>
                </int>
            </to>
        </path>
    </root>
    """

xmlDecoding = Xml.Decode.decodeString dataDecoder xmlTest


xmlTest2 = 
    """
    <ann>
        <anime id="4658" name="Jinki:Extend">23</anime>
    </ann>
    """

xmlTest3 = 
    """
    <ann>
        <anime id="4569">1</anime>
    </ann>
    """

xmTest3Decoder = 
    Xml.Decode.map (\x -> {anime = x})
        (Xml.Decode.path ["anime"] (Xml.Decode.single Xml.Decode.int))

type alias AnimeData = 
    {id : String
    ,name: String
    ,anime_int: Int
    }

xmlTest2Decoder = 
    Xml.Decode.map3 AnimeData 
        (Xml.Decode.path ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "id")))
        (Xml.Decode.path ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "name")))
        (Xml.Decode.path ["anime"] (Xml.Decode.single Xml.Decode.int))

xmlTest2Decoder2 = 
    Xml.Decode.map (\x -> {id = x}) 
        (Xml.Decode.path ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "id")))

xmlTest2Decoding2 = Xml.Decode.decodeString xmlTest2Decoder2 xmlTest2


xmlTest2Decoding = Xml.Decode.decodeString xmlTest2Decoder xmlTest2

myAttr : String -> Xml.Decode.Decoder String
myAttr = Xml.Decode.stringAttr



xmlTest3Decoding = Xml.Decode.decodeString xmTest3Decoder xmlTest3


---**************************************************************
---**********************Test 4********************************
xmlTest4 : String 
xmlTest4 = 
    """
    <ann>
        <info id="1">1</info>
        <info id="3">2</info>
        <info id="4">3</info>
        <info id="5">4</info>
        <info id="6">5</info>
        <info id="7">6</info>
        <info id="8">7</info>
        <info id="9">8</info>
    </ann>
    """

type alias Info = 
    { id : List String 
    , nums: List Int}

pipelineDecoder: Xml.Decode.Decoder Info 
pipelineDecoder = 
    Xml.Decode.succeed Info 
        |> Xml.Decode.requiredPath ["info"] (Xml.Decode.list (Xml.Decode.stringAttr "id"))
        |> Xml.Decode.requiredPath ["info"] (Xml.Decode.list Xml.Decode.int)

xmlTest4Decoding = Xml.Decode.decodeString pipelineDecoder xmlTest4



---**************************************************************
---**************************************************************
animeNewsNetwork : String 
animeNewsNetwork = "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?anime=4658"


xmlTest5 = 
    """
    <anime>
        <info><info>1</info></info>
        <info>2</info>
    </anime>
    """

type alias InfoTest = {ids : List Int}

myDecoder5 = Xml.Decode.map InfoTest 
                (Xml.Decode.path ["info"] (Xml.Decode.leakyList Xml.Decode.int))

mySingle = (Xml.Decode.path ["info"] (Xml.Decode.list Xml.Decode.int))
mySingle2 = (Xml.Decode.path ["info","info"] (Xml.Decode.single Xml.Decode.int))

decodingTest5 = Xml.Decode.decodeString myDecoder5 xmlTest5
decodingTest5_mySingle = Xml.Decode.decodeString mySingle xmlTest5
decodingTest5_mySingle2 = Xml.Decode.decodeString mySingle2 xmlTest5