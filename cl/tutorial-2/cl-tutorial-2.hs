----------------------------------
-- Exercise 2

data Thing = A | B | C | D | E deriving (Eq, Show)
things :: [Thing]

things = [A,B,C,D,E]

data Colour = Orange | Blue deriving (Eq, Show)
data Shape = Square | Circle deriving (Eq, Show)
data Size = Big | Small deriving (Eq, Show)
data Outline = Outline | NoOutline deriving (Eq, Show)

colour :: Thing -> Colour
shape :: Thing -> Shape
size :: Thing -> Size
outline :: Thing -> Outline

colour A = Orange
colour B = Orange
colour C = Orange
colour D = Blue
colour E = Orange

shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square

size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

outline A = Outline
outline B = NoOutline
outline C = Outline
outline D = Outline
outline E = Outline

-------------------
-- Exercise 3
type Predicate u = u -> Bool
isOrange :: Predicate Thing
isSquare :: Predicate Thing
isBig :: Predicate Thing
hasOutline :: Predicate Thing

isOrange x = x `elem` [A,B,C,E]
isSquare x = x `elem` [A,B,D,E]
isBig x = x `elem` [A,B,C,D]
hasOutline x = x `elem` [A,C,D,E]

-- Adding the opposite ones here..
isBlue :: Predicate Thing
isCircle :: Predicate Thing
isSmall :: Predicate Thing
noHasOutline :: Predicate Thing
isBlue x = not (isOrange x)
isCircle x = not (isSquare x)
isSmall x = not (isBig x)
noHasOutline x = not (hasOutline x)

----------------
-- Exercise 4
statement1 :: Bool
statement1 = and [hasOutline thing | thing <- things, isSquare thing && not (isOrange thing)]
-- Returns True

statement2 :: Bool
statement2 = or [not (isBig thing) | thing <- things, not (isSquare thing) && isOrange thing]
-- Returns False


------------
-- Exercise 5

firstStatement :: Bool
firstStatement = not (or [isBlue thing | thing <- things, isSquare thing])

secondStatement :: Bool
secondStatement = and [not (isBlue thing) | thing <- things, isSquare thing]


-------------
-- Exercise 6

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan thing = [x | x <- things, x /= thing]

properties :: [Predicate Thing]
properties = [isBlue, isOrange, isSquare, isCircle, isBig, isSmall, hasOutline, noHasOutline]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf x = [property | property <- properties, property x]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = or [p thing | thing <- things, thing /= x]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [property | property <- properties, not (isPropertyOfAnotherThing property x)]

rank :: Thing -> Int
rank x = length (propertiesOnlyOf x)

rankList :: [(Thing, Int)]
rankList = [(thing, rank thing) | thing <- things]
-- A stands out as it has zero unique properties!