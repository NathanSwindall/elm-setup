module PlayGround.Basics exposing (..)

--Simploe String


isKeepable character = character /= '-'
zKeepable = isKeepable 'z'

phoneNum = String.filter isKeepable "800-555-1234"

--Let's use a let-expression
letExpression str = let
                        dash = '-'
                        isKeepable2 character = character /= dash
                    in
                    String.filter isKeepable str


--Anonymous functions
isKeepable3 = \char -> char /= '-'


--Collections
myList = ["one fish", "two fish"]
myList_length = List.length myList
myList_first = List.head myList
myList_filter = List.filter (\char -> char /= '-') ['Z', '-', 'Z']

-- Elm records

myRecord = { name = "Li", cats = 2 }
myRecordCats = myRecord.cats

    --update a record
nathan = { name = "Nathan", age = 30}
nathan31 = { nathan | age = 31}

--Tuples (don't want to bother naming the field but want a record like structure)
myTuple = ("Tech", 9)
myTuple_first = Tuple.first ("Tech", 9)

    --Tuple deconstructing
multiply3d (x, y, z) = x * y * z 
