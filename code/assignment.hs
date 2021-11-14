import Data.List
import System.Random
import Data.Ord
import Debug.Trace
{-
    Assignment for COM2108 Functional Programming
    Jordan Pownall, November 2021
-}

--Step 1: Define Initial Datatypes
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Show, Enum)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Show, Enum)

type Card = (Suit, Pip)

type Deck = [Card]

--Step 2: Implement Basic Functionality

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
shuffle n = [card | (card,n) <- sortBy cmp (zip pack ((randoms (mkStdGen n)) :: [Int]))]

--Step 3: Define datatypes to represent an eight-off board

type Foundation = [Deck] 

type Column = [Deck]

type Reserve = [Card]

type EOBoard = (Foundation, Column, Reserve)

--Step 4: Implement further functionality
-- function to initially split the deck into cells (reserve) (should start with 4 random cards in 8 cells), 
-- foundation (4 empty cells) and tableau (columns) (8 places, each with 6 cards initially)
-- basically sets up (deals) a new game
eODeal :: Int -> EOBoard
eODeal n = (foundation, column, reserve) where
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
    | otherwise = (take 6 deck):(splitUp (drop 6 deck)) -- recursively calls to split up deck into 6 every time until empty set


-- shows the board
eOBoardShow :: EOBoard -> String 
eOBoardShow (foundation, column, reserve) =
    "Foundations: " ++ show foundation
    ++  "Columns: " ++ show column
    ++ "Reserve: " ++ show reserve

-- moves cards possible cards from tableau to foundation 
toFoundations :: EOBoard -> EOBoard
toFoundations board@(foundation, column, reserve)
    | any (toFoundationsHelper foundation) (getHeads column ++ reserve) = toFoundations newBoard --checks if any of the cards at the head of column and reserve meet helper, if so move card
    | otherwise = board     --otherwise do nothing
    where
        newBoard = foldr (moveAceFoundations) board (getHeads column ++ reserve)
    

-- checks if the card can move (essentially if it is an Ace or if its predecessor of the head of a list in foundations)
toFoundationsHelper :: [Deck] -> Card -> Bool
toFoundationsHelper foundation c = isAce c || elem (pCard c) (map head foundation)

-- finds heads from each list to evaluate
getHeads :: [Deck] -> Deck
getHeads [] = []
getHeads (x:xs) = head x : getHeads xs

-- removes the card from a stack if it is found at the head of any of the lists
removeHead :: Card -> [Deck] -> [Deck]
removeHead c [] = []
removeHead c (x:xs)
    | c == head x = delete c x:xs                 -- if c is at the head of this list, delete
    | otherwise = x:removeHead c xs               -- if not, repeat process for rest of the list until found

-- this function checks if the ace card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation and removes it from its current position
moveAceFoundations :: Card -> EOBoard -> EOBoard
moveAceFoundations c board@(foundation, column, reserve)
    | isAce c = (([c]:foundation), removeHead c column, delete c reserve)   --if the card is an ace it is added to foundations (empty list)
    | otherwise = moveCardFoundations c board         --this way the function can be called every time in case theres an ace, if not will check for rest of cards

-- this function checks if the card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation it belongs to and removes it from its current position
moveCardFoundations :: Card -> EOBoard -> EOBoard
moveCardFoundations c ([], column, reserve) = ([], column, reserve)
moveCardFoundations c board@(x:xs, column, reserve)
    | head x == pCard c = (((c:x):xs), removeHead c column, delete c reserve) -- checks if head of each foundation is the pCard of c
    | otherwise = ((x:newFound), newColumn, newReserve)    -- calls recursively if the card is not found to be able to move from current column
    where
        (newFound, newColumn, newReserve) = moveCardFoundations c (xs, column, reserve)

{-
-- checks if a card can be moved to the reserves
canMoveToReserves :: EOBoard -> Bool
canMoveToReserves (_,_,reserves)
    | length reserves < 8 = True        -- if the length of the reserves list is less than 8, a card can be added
    | otherwise = False

-- moves a card to the reserves and deletes it from the column it was in
moveCardToReserves :: Card -> EOBoard -> EOBoard
moveCardToReserves c board@(foundation, column, reserve)
    | canMoveToReserves board = (foundation, removeHead c column, c:reserve)   -- updates board with deleted card from tableau and adds to reserve
    | otherwise = board

-- checks if a successor card is the head of any of the lists in the tableau
toColumnsHelper :: [Deck] -> Card -> Bool
toColumnsHelper column c = elem (sCard c) (map head column)

-- checks if a card can be moved to the tableau
canMoveToColumn :: EOBoard -> Bool
canMoveToColumn (_,column,_)
    | any (toColumnsHelper column) (getHeads column) = True
    | otherwise = False

-- moves a card to the tableau and deletes it from the reserves
moveCardToColumn :: Card -> EOBoard -> EOBoard
moveCardToColumn c board@(foundation, x:xs, reserve)
    | head x == sCard c = (foundation, ((c:x):xs), delete c reserve) -- checks if head of a column is the sCard of c, if so add c
    | otherwise = (newFound, (x:newColumn), newReserve)    -- calls recursively if the sCard is not found at the head of that column
    where
        (newFound, newColumn, newReserve) = moveCardToColumn c (foundation, xs, reserve)
-}
