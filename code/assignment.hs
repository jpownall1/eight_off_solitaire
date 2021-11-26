{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import System.Random
import Data.Ord
import Debug.Trace
{-
    Assignment for COM2108 Functional Programming
    Jordan Pownall, November 2021
-}

--------------------------------------------------------------------------PART 1----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------Step 1: Define Initial Datatypes-----------------------------------------------------------------------------------------------------
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Show, Enum)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Show, Enum)

type Card = (Suit, Pip)

type Deck = [Card]

--------------------------------------------------------------Step 2: Implement Basic Functionality---------------------------------------------------------------------------------------------------

--list of all cards in a deck (apart from Jokers)
pack = [(Hearts, Ace), (Hearts, Two), (Hearts, Three), (Hearts, Four), (Hearts, Five), (Hearts, Six),
    (Hearts, Seven), (Hearts, Eight), (Hearts, Nine), (Hearts, Ten), (Hearts, Jack), (Hearts, Queen), (Hearts, King),
    (Clubs, Ace), (Clubs, Two), (Clubs, Three), (Clubs, Four), (Clubs, Five), (Clubs, Six),
    (Clubs, Seven), (Clubs, Eight), (Clubs, Nine), (Clubs, Ten), (Clubs, Jack), (Clubs, Queen), (Clubs, King),
    (Spades, Ace), (Spades, Two), (Spades, Three), (Spades, Four), (Spades, Five), (Spades, Six),
    (Spades, Seven), (Spades, Eight), (Spades, Nine), (Spades, Ten), (Spades, Jack), (Spades, Queen), (Spades, King),
    (Diamonds, Ace), (Diamonds, Two), (Diamonds, Three), (Diamonds, Four), (Diamonds, Five), (Diamonds, Six),
    (Diamonds, Seven), (Diamonds, Eight), (Diamonds, Nine), (Diamonds, Ten), (Diamonds, Jack), (Diamonds, Queen), (Diamonds, King)]

--functions to return successor card
sPip :: Pip -> Pip
sPip King = Ace                         --if King is called, return Ace
sPip a = succ a                         --otherwise, return successor card in data Pip

sCard :: Card -> Card
sCard x = (fst x, sPip (snd x))

--functions to return predecessor card value
pPip :: Pip -> Pip
pPip Ace = King                        --if Ace is called, return King
pPip a = pred a                        --otherwise, return predecessor card in data Pip

--returns predecessor of card
pCard :: Card -> Card
pCard x = (fst x, pPip (snd x))

--checks if the card is a king
isKing :: Card -> Bool
isKing x = snd x == King               --checks for second item in card tuple, uses deriving Eq to check if it is a King

--checks if the card is an Ace
isAce :: Card -> Bool
isAce x = snd x == Ace                 --checks for second item in card tuple, uses deriving Eq to check if it is an Ace

--shuffles cards
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle :: Int -> Deck
shuffle n = [card | (card,n) <- sortBy cmp (zip pack (randoms (mkStdGen n) :: [Int]))]

--------------------------------------------------------Step 3: Define datatypes to represent an eight-off board--------------------------------------------------------------------------------------
-----------------------------------------------------STEP 5: DEFINE DATATYPES TO REPRESENT A FOUR SUIT SPIDER BOARD-----------------------------------------------------------------------------------
type Stock = Deck

type Foundation = [Deck]

type Column = [Deck]

type Reserve = [Card]

data Board = EOBoard (Foundation, Column, Reserve) | SBoard (Foundation, Column, Stock) deriving (Eq)
{-
instance Show Board where
    show (EOBoard foundation column reserve) = "Foundations: " ++ show foundation ++  "Columns: " ++ show column ++ "Reserve: " ++ show reserve
    show (SBoard foundation column stock) = "Foundations: " ++ show foundation ++  "Columns: " ++ show column ++ "Stock: " ++ show stock
-}
----------------------------------------------------------------Step 4: Implement further functionality-----------------------------------------------------------------------------------------------
-- function to initially split the deck into cells (reserve) (should start with 4 random cards in 8 cells), 
-- foundation (4 empty cells) and tableau (columns) (8 places, each with 6 cards initially)
-- basically sets up (deals) a new game
eODeal :: Int -> Board
eODeal n = EOBoard (foundation, column, reserve) where
    shuffledDeck = shuffle n                     --shuffle cards using seed n
    foundation = []                              --initialises empty array for foundation
    reserve = take 4 shuffledDeck                --takes the 4 first random cards from shuffled deck for reserve list
    column = splitUp (drop 4 shuffledDeck)       --splits the remaining cards into 8 equal lists for tableau

-- Splits remaining cards (not in reserves) into 6 equal piles in tableau using recursion taking
-- 6 cards a time from input 'deck' for 8 lists in tabeleau
splitUp :: Deck -> [Deck]
splitUp [] = []
splitUp deck
    | length deck < 6 = []
    | otherwise = take 6 deck:splitUp (drop 6 deck) -- recursively calls to split up deck into 6 every time until empty set

-- moves cards possible cards from tableau and reserve to foundation 
toFoundations :: Board -> Board
toFoundations (EOBoard board@(foundation, column, reserve))
    | canMoveToFoundations (EOBoard board) = toFoundations newBoard --checks if can move, if so moves the card
    | otherwise = (EOBoard board)                                   --otherwise do nothing
    where
        newBoard = foldr moveAceFoundations (EOBoard board) (getHeads column ++ reserve)


-- checks if the card can move (essentially if it is an Ace or if its predecessor of the head of a list in foundations)
toFoundationsHelper :: [Deck] -> Card -> Bool
toFoundationsHelper foundation c = isAce c || elem (pCard c) (map head foundation)

-- checks if any cards at the head of column and reserve meet helper i.e. can be moved to foundations or not
canMoveToFoundations :: Board -> Bool
canMoveToFoundations (EOBoard board@(foundation, column, reserve)) = any (toFoundationsHelper foundation) (getHeads column ++ reserve)

-- finds heads from each list to evaluate
getHeads :: [Deck] -> Deck
getHeads xs = map head xs

-- removes the card from a stack if it is found at the head of any of the lists
removeHead :: Card -> [Deck] -> [Deck]
removeHead c [] = []
removeHead c (x:xs)
    | c == head x = delete c x:xs                 -- if c is at the head of this list, delete
    | otherwise = x:removeHead c xs               -- if not, repeat process for rest of the list until found

-- this function checks if the ace card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation and removes it from its current position
moveAceFoundations :: Card -> Board -> Board
moveAceFoundations c (EOBoard board@(foundation, column, reserve))
    | isAce c = EOBoard ([c]:foundation, removeHead c column, delete c reserve)   --if the card is an ace it is added to foundations (empty list)
    | otherwise = moveCardFoundations c (EOBoard board)         --this way the function can be called every time in case theres an ace, if not will check for rest of cards

-- this function checks if the card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation it belongs to and removes it from its current position
moveCardFoundations :: Card -> Board -> Board
moveCardFoundations c (EOBoard ([], column, reserve)) = (EOBoard ([], column, reserve))
moveCardFoundations c (EOBoard board@(x:xs, column, reserve))
    | head x == pCard c = EOBoard ((c:x):xs, removeHead c column, delete c reserve) -- checks if head of each foundation is the pCard of c
    | otherwise = EOBoard (x:newFound, newColumn, newReserve)    -- calls recursively if the card is not found to be able to move from current column
    where
        (EOBoard (newFound, newColumn, newReserve)) = moveCardFoundations c (EOBoard (xs, column, reserve))

--------------------------------------------------------------------------PART 2----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------Step 1: A FUNCTION TO FIND ALL POSSIBLE MOVES FOR EIGHT-OFF -----------------------------------------------------------------------------------
-- checks if a card can be moved to the reserves
canMoveToReserves :: Board -> Bool
canMoveToReserves (EOBoard board@(_,_,reserves))
    | length reserves < 8 = True        -- if the length of the reserves list is less than 8, a card can be added
    | otherwise = False

-- moves a card to the reserves and deletes it from the column it was in
moveCardToReserves :: Card -> Board -> Board
moveCardToReserves c (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = EOBoard(foundation, removeHead c column, c:reserve)   -- updates board with deleted card from tableau and adds to reserve
    | otherwise = EOBoard board

--cards that can move from the column to the reserve
getColumnMovables :: Board -> [Card]
getColumnMovables (EOBoard board@(_,[],_)) = []
getColumnMovables (EOBoard board@(foundation,x:xs,reserve))
    | canMoveToColumn (EOBoard board) = head x : getColumnMovables (EOBoard (foundation, xs, reserve))
    | otherwise = []

-- this method finds the Board array from possible moves to the reserves
difReservesMoves :: Board -> [Board]
difReservesMoves (EOBoard board@(foundation, [], reserve)) = []
difReservesMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = removeItem (EOBoard board) boards
    | otherwise = []
    where
        movables = getColumnMovables (EOBoard board)
        boards =
            [if length movables < 2 then moveCardToReserves (movables!!0) (EOBoard board) else (EOBoard board),
                if length movables < 3 then moveCardToReserves (movables!!1) (EOBoard board) else (EOBoard board),
                    if length movables < 4 then moveCardToReserves (movables!!2) (EOBoard board) else (EOBoard board),
                        if length movables < 5 then moveCardToReserves (movables!!3) (EOBoard board) else (EOBoard board),
                            if length movables < 6 then moveCardToReserves (movables!!4) (EOBoard board) else (EOBoard board),
                                if length movables < 7 then moveCardToReserves (movables!!5) (EOBoard board) else (EOBoard board),
                                    if length movables < 8 then moveCardToReserves (movables!!6) (EOBoard board) else (EOBoard board),
                                        if length movables < 9 then moveCardToReserves (movables!!7) (EOBoard board) else (EOBoard board)]

-- checks if a successor card is the head of any of the lists in the tableau
toColumnsHelper :: [Deck] -> Card -> Bool
toColumnsHelper column c = (sCard c) `elem` (map head column)

-- checks if a card can be moved to the tableau
canMoveToColumn :: Board -> Bool
canMoveToColumn (EOBoard (_,column,_))
    | any (toColumnsHelper column) (getHeads column) = True
    | otherwise = False

-- moves a card to the tableau and deletes it from the reserves
moveCardToColumn :: Card -> Board -> Board
moveCardToColumn c (EOBoard board@(foundation, [], reserve)) = EOBoard (foundation, [[c]], reserve)
moveCardToColumn c (EOBoard board@(foundation, x:xs, reserve))
    | head x == sCard c = EOBoard (foundation, (c:x):xs, delete c reserve) -- checks if head of a column is the sCard of c, if so add c
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)              -- calls recursively if the sCard is not found at the head of that column
    where
        EOBoard (newFound, newColumn, newReserve) = moveCardToColumn c (EOBoard (foundation, xs, reserve))

-- returns cards that can move from the reserve to the tableau
getReserveMovablesMayb :: Eq a => Board -> [Maybe a]
getReserveMovablesMayb (EOBoard board@(_,_,[])) = []
getReserveMovablesMayb (EOBoard board@(foundation,column,reserves))
    | canMoveToColumn (EOBoard board) = removeItem Nothing movables
    | otherwise = []
    where
        columnHeads = getHeads column
        moveables =
            [if (length reserves) < 2 && (toColumnsHelper column (reserves!!0)) then Just (reserves!!0) else Nothing,
                if (length reserves) < 3 && (toColumnsHelper column (reserves!!1)) then Just (reserves!!1) else Nothing,
                    if (length reserves) < 4 && (toColumnsHelper column (reserves!!2)) then Just (reserves!!2) else Nothing,
                        if (length reserves) < 5 && (toColumnsHelper column (reserves!!3)) then Just (reserves!!3) else Nothing,
                            if (length reserves) < 6 && (toColumnsHelper column (reserves!!4)) then Just (reserves!!4) else Nothing,
                                if (length reserves) < 7 && (toColumnsHelper column (reserves!!5)) then Just (reserves!!5) else Nothing,
                                    if (length reserves) < 8 && (toColumnsHelper column (reserves!!6)) then Just (reserves!!6) else Nothing,
                                        if (length reserves) < 9 && (toColumnsHelper column (reserves!!7)) then Just (reserves!!7) else Nothing]

-- had to define this method for the above method to work
movables :: [Maybe a]
movables = error "not implemented"

-- defined this function to be used in the next function as a way to retrieve the cards what can move from the reserves to tableau
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Still nothings in your array."

-- retrieves the cards from Just a to a
getReserveMovables :: Board -> [Card]
getReserveMovables (EOBoard board) = map fromJust (getReserveMovablesMayb (EOBoard board))


-- this method finds the Board array from possible moves from columns to the reserves
difColumnMoves :: Board -> [Board]
difColumnMoves (EOBoard board@(foundation, [], reserve)) = []
difColumnMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = removeItem (EOBoard board) boards
    | otherwise = []
    where
        movables = getReserveMovables (EOBoard board)
        boards =
            [if length movables < 2 then moveCardToColumn (movables!!0) (EOBoard board) else (EOBoard board),
                if length movables < 3 then moveCardToColumn (movables!!1) (EOBoard board) else (EOBoard board),
                    if length movables < 4 then moveCardToColumn (movables!!2) (EOBoard board) else (EOBoard board),
                        if length movables < 5 then moveCardToColumn (movables!!3) (EOBoard board) else (EOBoard board),
                            if length movables < 6 then moveCardToColumn (movables!!4) (EOBoard board) else (EOBoard board),
                                if length movables < 7 then moveCardToColumn (movables!!5) (EOBoard board) else (EOBoard board),
                                    if length movables < 8 then moveCardToColumn (movables!!6) (EOBoard board) else (EOBoard board),
                                        if length movables < 9 then moveCardToColumn (movables!!7) (EOBoard board) else (EOBoard board)]

--This method is for filtering, used in many functions
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- This method finds all the possible moves in the game, with toFoundations called on each of the boards as instructed.
findMoves :: Board -> [Board]
findMoves (EOBoard board@(foundation,column,reserves))
    | removeItem (EOBoard board) (map toFoundations (difReservesMoves (EOBoard board) ++ difColumnMoves (EOBoard board) ++ [(EOBoard board)])) == [] = []
    | otherwise = map toFoundations (difReservesMoves (EOBoard board) ++ difColumnMoves (EOBoard board) ++ [(EOBoard board)])

---------------------------------------------------------------Step 2: A FUNCTION TO CHOOSE THE NEXT MOVE---------------------------------------------------------------------------------------------
--I have chosen to choose which move based on which one has the biggest number of cards in foundations.

-- finds the amount of cards in foundations for a given board
foundationSize :: Board -> Int 
foundationSize (EOBoard board@([],_,_)) = 0
foundationSize (EOBoard board@(x:xs,foundation,reserve)) = length x + foundationSize (EOBoard (xs,foundation,reserve))

-- finds the board with the biggest bumber of cards in foundations
foundationBiggest :: [Board] -> Board
foundationBiggest = foldr1 (\x y -> if foundationSize x >= foundationSize y then x else y)

--This method chooses the move 
chooseMove :: Board -> Maybe Board
chooseMove (EOBoard board@(foundation, column, reserve))
    | findMoves (EOBoard board) == [] = Nothing
    | otherwise = Just biggestFoundation
    where
        biggestFoundation = foundationBiggest (findMoves (EOBoard board))

-------------------------------------------------------Step 3: A FUNCTION TO PLAY A GAME OF EIGHT-OFF SOLITAIRE---------------------------------------------------------------------------------------
haveWon :: Board -> Bool
haveWon (EOBoard board@(foundation, column, reserve))
    | column == [] && reserve == [] = True 
    | otherwise = False 
