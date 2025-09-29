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

isBlue :: Predicate Thing
isBlue x = not (isOrange x)

firstStatement :: Bool
firstStatement = not (or [isBlue thing | thing <- things, isSquare thing])

secondStatement :: Bool
secondStatement = and [not (isBlue thing) | thing <- things, isSquare thing]

