{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import System.Random
import Data.Ord
import Debug.Trace
import Data.Maybe
import Data.Fixed
{-
    Assignment for COM2108 Functional Programming
    Jordan Pownall, December 2021
-}

--------------------------------------------------------------------------PART 1----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------Step 1: Define Initial Datatypes-----------------------------------------------------------------------------------------------------

-- A datatype to represent the different suits of a card in a standard deck
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Show, Enum)

-- A datatype to represent the different possible values of a card in a standard deck
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Show, Enum)

-- A combination of the two above datatypes, representing a card
type Card = (Suit, Pip)

-- A Deck datatype, which is a list of Cards
type Deck = [Card]

--------------------------------------------------------------Step 2: Implement Basic Functionality---------------------------------------------------------------------------------------------------

-- A list of all cards in a standard deck (apart from Jokers)
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
sPip a = succ a                         --otherwise, return successor Pip value

sCard :: Card -> Card
sCard x = (fst x, sPip (snd x))

--functions to return predecessor card value
pPip :: Pip -> Pip
pPip Ace = King                        --if Ace is called, return King
pPip a = pred a                        --otherwise, return predecessor Pip value

--returns predecessor of card
pCard :: Card -> Card
pCard x = (fst x, pPip (snd x))

--checks if the card is a king
isKing :: Card -> Bool
isKing x = snd x == King               --checks for Pip value in card tuple, uses deriving Eq to check if it is a King

--checks if the card is an Ace
isAce :: Card -> Bool
isAce x = snd x == Ace                 --checks for Pip value in card tuple, uses deriving Eq to check if it is an Ace

------------------------------------------------------------------------------More Utilities-----------------------------------------------------------------------------------------------------------

-- This is a method used for comparisons. It helps with the following two methods, for shuffling
cmp (x1,y1) (x2,y2) = compare y1 y2

-- This method shuffles a deck dependent a user defined seed
shuffle :: Int -> Deck
shuffle n = [card | (card,n) <- sortBy cmp (zip pack (randoms (mkStdGen n) :: [Int]))]

-- finds card heads from each list in a list of lists to evaluate
getHeads :: [Deck] -> Deck
getHeads [] = []
getHeads (x:xs)
    | null x = getHeads xs
    | otherwise = head x : getHeads xs

-- finds cards behind the heads from each list in a list of lists to evaluate
getSecondHeads :: [Deck] -> Deck
getSecondHeads [] = []
getSecondHeads (x:xs)
    | null x = getSecondHeads xs
    | otherwise = if isNothing (getSecondHead x) then getSecondHeads xs else fromJust (getSecondHead x) : getSecondHeads xs

-- finds second card from a list to evaluate
getSecondHead :: Deck -> Maybe Card
getSecondHead [] = Nothing
getSecondHead (x:xs)
    | null xs = Nothing
    | otherwise = Just (head xs)

-- removes the card from a stack if it is found at the head of any of the lists
removeHead :: Card -> [Deck] -> [Deck]
removeHead c [] = []                              -- base case for when the recursion is done 
removeHead c (x:xs)
    | x == [] = x:removeHead c xs                 -- if a list is empty, return an empty list
    | c == head x = removeItem c x : xs           -- if c is at the head of this list, delete
    | otherwise = x:removeHead c xs               -- if not, repeat process for rest of the list until found

--This method is for filtering, removing an item from a list, used in many functions
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

--This method is for filtering, removes all items in a list from another list, used in many functions
removeItems :: Eq a => [a] -> [a] -> [a]
removeItems _ []                 = []
removeItems [] a                 = a
removeItems (x:xs) list          = removeItem x (removeItems xs list)

--------------------------------------------------------Step 3: Define datatypes to represent an eight-off board--------------------------------------------------------------------------------------
-----------------------------------------------------STEP 5: DEFINE DATATYPES TO REPRESENT A FOUR SUIT SPIDER BOARD-----------------------------------------------------------------------------------
type Stock = Deck

type Foundation = [Deck]

type Column = [Deck]

type Reserve = [Card]

data Board = EOBoard (Foundation, Column, Reserve) | SBoard (Foundation, Column, Stock) deriving (Eq)

instance Show Board where
    show (EOBoard (foundation, column, reserve)) = "Foundations: " ++ "\n" ++ show foundation ++ "\n" ++ "Columns: " ++ "\n" ++ show column ++ "\n" ++ "Reserve: " ++ "\n" ++ show reserve ++ "\n"
    show (SBoard (foundation, column, stock)) = "Foundations: " ++ "\n" ++ show foundation ++ "\n" ++  "Columns: " ++ "\n" ++ show column ++ "\n" ++ "Stock: " ++ "\n" ++ show stock ++ "\n"

----------------------------------------------------------------Step 4: Implement further functionality-----------------------------------------------------------------------------------------------
{- function to initially split the deck into cells (reserve) (should start with 4 random cards in 8 cells), 
   foundation (4 empty cells) and tableau (columns) (8 places, each with 6 cards initially) basically sets up (deals) a new game -}
eODeal :: Int -> Board
eODeal n = EOBoard (foundation, column, reserve) where
    shuffledDeck = shuffle n                     --shuffle cards using seed n
    foundation = []                              --initialises empty array for foundation
    reserve = take 4 shuffledDeck                --takes the 4 first random cards from shuffled deck for reserve list
    column = splitUpEO (drop 4 shuffledDeck)       --splits the remaining cards into 8 equal lists for tableau

{- Splits remaining cards (not in reserves) into 6 equal piles in tableau using recursion taking
   6 cards a time from input 'deck' for 8 lists in tabeleau -}
splitUpEO :: Deck -> Column
splitUpEO [] = []
splitUpEO deck
    | length deck < 6 = []
    | otherwise = take 6 deck:splitUpEO (drop 6 deck) -- recursively calls to split up deck into 6 every time until empty set

-------------------------------The following 5 methods are for moving cards to foundations from the reserves or the tableau---------------------------------------------------------------------------

-- checks if the card can move to foundations from the reserve or tableau (essentially if it is an Ace or if its predecessor of the head of a list in foundations)
toFoundationsHelper :: Foundation -> Card -> Bool
toFoundationsHelper foundation c = isAce c || elem (pCard c) (getHeads foundation)

-- checks if any cards at the head of column and reserve meet helper i.e. can be moved to foundations or not
canMoveToFoundations :: Board -> Bool
canMoveToFoundations (EOBoard board@(foundation, [], [])) = False
canMoveToFoundations (EOBoard board@(foundation, column, [])) = any (toFoundationsHelper foundation) (getHeads column)
canMoveToFoundations (EOBoard board@(foundation, [], reserve)) = any (toFoundationsHelper foundation) reserve
canMoveToFoundations (EOBoard board@(foundation, column, reserve)) = any (toFoundationsHelper foundation) (getHeads column ++ reserve)

{- this function checks if the ace card (c) from any of the reserves or the heads of the columns 
   can be moved, if so adds to the foundation and removes it from its current position. If no aces
   are found, the moveCardFoundations will be called -}
moveAceFoundations :: Card -> Board -> Board
moveAceFoundations c (EOBoard board@(foundation, column, reserve))
    | isAce c = EOBoard ([c]:foundation, removeHead c column, removeItem c reserve)   -- if the card is an ace it is added to foundations (empty list)
    | otherwise = moveCardFoundations c (EOBoard board)                               -- this way if an ace isnt found, it will will check for rest of cards

{- this function checks if the card (c) from any of the reserves or the heads of the columns 
   can be moved, if so adds to the foundation it belongs to and removes it from its current position -}
moveCardFoundations :: Card -> Board -> Board
moveCardFoundations c (EOBoard ([], column, reserve)) = EOBoard ([], column, reserve)
moveCardFoundations c (EOBoard board@(x:xs, column, reserve))
    | null x = EOBoard (x:newFound, newColumn, newReserve)
    | head x == pCard c = EOBoard ((c:x):xs, removeHead c column, removeItem c reserve) -- checks if head of each foundation is the pCard of c
    | otherwise = EOBoard (x:newFound, newColumn, newReserve)                           -- calls recursively if the card is not found to be able to move from current column
    where
        (EOBoard (newFound, newColumn, newReserve)) = moveCardFoundations c (EOBoard (xs, column, reserve))

-- This method combines the two above, testing on all of the cards that can move if they can move to the foundations
toFoundations :: Board -> Board
toFoundations (EOBoard board@(foundation, column, reserve))
    | canMoveToFoundations (EOBoard board) = toFoundations newBoard        --checks there can be any moves to the foundations, if so moves the card
    | otherwise = EOBoard board                                            --otherwise do nothing and return the same board
    where
        newBoard = foldr moveAceFoundations (EOBoard board) (getHeads column ++ reserve)

----------------------------------------------------------------------STEP 6: ONE FINAL FUNCTION------------------------------------------------------------------------------------------------------

{- function to initially split the deck into column, which is just the first 54 cards split up into sets of 6x4 and 5x6
   for the columnsand the rest of the 50 cards are put into stock - basically sets up (deals) a new game fo Spider Solitaire -}
sDeal :: Int -> Board
sDeal n = SBoard (foundation, column, stock) where
    shuffledDeck = shuffle n ++ shuffle (n+1)                     --shuffle cards using seed n
    foundation = []                                               --initialises empty array for foundation
    column = spiderSplitUp (take 54 shuffledDeck)                 --splits the remaining cards into 10 lists, 6x4 and 5x6
    stock = drop 54 shuffledDeck                                  --takes the 4 first random cards from shuffled deck for reserve list

{- Splits remaining cards (not in reserves) into 6 equal piles in tableau using recursion taking
   6 cards a time from input 'deck' for 8 lists in tabeleau -}
spiderSplitUp :: Deck -> Column
spiderSplitUp [] = []
spiderSplitUp deck
    | length deck < 5 = []
    | length deck > 30 = take 6 deck:spiderSplitUp (drop 6 deck) -- recursively calls to split up deck into 6 until the deck is less than 30 cards
    | otherwise = take 5 deck:spiderSplitUp (drop 5 deck)        -- then split into 5 

--------------------------------------------------------------------------PART 2----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------Step 1: A FUNCTION TO FIND ALL POSSIBLE MOVES FOR EIGHT-OFF------------------------------------------------------------------------------------

---------------------------------This part uses functions to find the possible moves from the columns (tableau) to the reserves-----------------------------------------------------------------------

-- checks if any cards at the head of the columns can be moved to the reserves
canMoveToReserves :: Board -> Bool
canMoveToReserves (EOBoard board@(_,_,reserves))
    | length reserves < 8 = True                 -- if the length of the reserves list is less than 8, a card can be added
    | otherwise = False

-- moves a card to the reserves and deletes it from the column it was in
moveCardToReserves :: Card -> Board -> Board
moveCardToReserves c (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = EOBoard(foundation, removeHead c column, c:reserve)   -- updates board with deleted card from tableau and adds to reserve
    | otherwise = EOBoard board

-- this method finds the Board array from possible moves to the reserves from all the cards at the head of the columns
difReservesMoves :: Board -> [Board]
difReservesMoves (EOBoard board@(foundation, [], reserve)) = [toFoundations (EOBoard board)]
difReservesMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = removeItem (EOBoard board) boards              -- removes moves what result in no move 
    | otherwise = []
    where
        boards = map (\x -> moveCardToReserves x (EOBoard board)) (getHeads column)      -- maps all column heads onto function to get the boards moving from column to reserves

---------------------------------------This part uses functions to find the possible moves from the reserves to the columns (tableau)----------------------------------------------------------------

-- checks if a successor card to inputted card is the head of any of the lists in the tableau, if so it can move there
toColumnsHelper :: Column -> Card -> Bool
toColumnsHelper column c = sCard c `elem` getHeads column

-- This function checks if any of the columns are empty, if so, cards can move from reserves to columns
emptyColumn :: Column -> Bool
emptyColumn [] = False
emptyColumn (x:xs)
    | null x = True                     -- if the oolumn is empty, return true
    | otherwise = emptyColumn xs        -- otherwise, perform check on the rest of the columns

-- checks if a card can be moved to the tableau
canMoveToColumn :: Board -> Bool
canMoveToColumn (EOBoard (_,column,reserve))
    | emptyColumn column = True                             -- if a column is empty, there can be a move to the columns
    | any (toColumnsHelper column) reserve = True           -- if toColumnsHelper for any card in the reserves is true, there can be a move to the columns
    | otherwise = False

-- moves a card to the tableau and deletes it from the reserves
moveCardToColumn :: Card -> Board -> Board
moveCardToColumn c (EOBoard board@(foundation, [], reserve)) = EOBoard board   -- if the tableau is empty then just return board as you should be able to move all cards to c foundations then
moveCardToColumn c (EOBoard board@(foundation, x:xs, reserve))
    | null x = EOBoard (newFound, x:newColumn, newReserve)
    | head x == sCard c = EOBoard (foundation, (c:x):xs, removeItem c reserve) -- checks if head of a column is the sCard of c, if so add c
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)                  -- calls recursively if the sCard is not found at the head of that column
    where
        EOBoard (newFound, newColumn, newReserve) = moveCardToColumn c (EOBoard (foundation, xs, reserve))

{- moves a king to the empty column in the tableau and deletes it from the reserves. This is separate to the above
   as this move should be made before the above as this is ranked higher in priorities -}
moveKingToEmptyColumn :: Card -> Board -> Board
moveKingToEmptyColumn c (EOBoard board@(foundation, [], reserve)) = EOBoard board   --if the tableau is empty then just return board as you should be able to move all cards to foundations then
moveKingToEmptyColumn c (EOBoard board@(foundation, x:xs, reserve))
    | null x && isKing c = EOBoard (foundation, (c:x):xs, removeItem c reserve)     -- checks if head of a column is empty, if so add king card c
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)                       -- calls recursively if the column is not found to be empty
    where
        EOBoard (newFound, newColumn, newReserve) = moveKingToEmptyColumn c (EOBoard (foundation, xs, reserve))

{- this method finds the Board array from possible moves from reserves to the columns moreBoards includes
   the moves to the empty columns. This is because if there is an empty column, boards throws an error -}
difColumnMoves :: Board -> [Board]
difColumnMoves (EOBoard board@(foundation, [], reserve)) = []
difColumnMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToColumn (EOBoard board) && emptyColumn column = removeItem (EOBoard board) (kingMove : boards)  -- all moves if a column is empty and canMoveToColumn
    | canMoveToColumn (EOBoard board) && not (emptyColumn column) = removeItem (EOBoard board) boards         -- if a column isnt empty, just moves without kings
    | otherwise = []
    where
        kingMove = if isNothing (getKingFromList reserve) then EOBoard board else moveKingToEmptyColumn (fromJust (getKingFromList reserve)) (EOBoard board)  -- moves from king to empty
        boards = map (\x -> moveCardToColumn x (EOBoard board)) reserve                                                                                       -- all other moves to columns                                                         

-------------------------------------------------------------This part is for cards moving from a column to a different column------------------------------------------------------------------------
-- This returns the first instance of a king if in a list. Useful for reserves -> columns and columns -> columns
getKingFromList :: Deck -> Maybe Card
getKingFromList [] = Nothing
getKingFromList (x:xs)
    | isKing x = Just x
    | otherwise = getKingFromList xs

-- This returns the king at a head of a column that arent already in their own column
getKingsAtColumnHead :: Column -> Deck
getKingsAtColumnHead [] = []
getKingsAtColumnHead (x:xs)
    | null x = getKingsAtColumnHead xs                                        -- If empty column, continue searching
    | (length x > 1) && isKing (head x) = head x : getKingsAtColumnHead xs    -- if theres a king (not in its own column) add king to returning list
    | otherwise = getKingsAtColumnHead xs                                     -- recursion

-- This method is for moving a card from a column to a different column
colToColKingMove :: Card -> Board -> Board
colToColKingMove c (EOBoard board@(foundation, [], reserve)) = EOBoard board          -- if the tableau is empty then just return board as no moves can be made for this method
colToColKingMove c (EOBoard board@(foundation, x:xs, reserve))
    | null x && isKing c = EOBoard (foundation, (c:x) : removeHead c xs, reserve)     -- checks if head of a column is empty, if so add king and delete from head of column
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)                         -- calls recursively until no more columns
    where
        EOBoard (newFound, newColumn, newReserve) = colToColKingMove c (EOBoard (foundation, xs, reserve))

-- This method is for moving a card from a column to a different column
colToColMove :: Card -> Board -> Board
colToColMove c (EOBoard board@(foundation, [], reserve)) = EOBoard board   -- if the tableau is empty then just return board as no moves can be made for this method
colToColMove c (EOBoard board@(foundation, x:xs, reserve))
    | null x && isKing c = colToColKingMove c (EOBoard board)              -- moves kings to empty column
    | null x = EOBoard (newFound, x:newColumn, newReserve)                 -- if empty column and card isnt a king, proceed with search for sCard
    | (length x > 1) && toColumnsHelper (x:xs) c && head x == sCard c = EOBoard (foundation, (c:x) : removeHead c newColumn, reserve)-- checks if head of a column is sCard, if so add card and delete from head of its current column
    | (length x > 1) && toColumnsHelper (x:xs) c && c `elem` x = EOBoard (newFound, delete c x:newColumn, newReserve) -- if c can move to another column and it is a member of the tested column, delete from that column and continue search
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)              -- calls recursively until no more columns
    where
        EOBoard (newFound, newColumn, newReserve) = colToColMove c (EOBoard (foundation, xs, reserve))

-- Checks if column to column moves can be made, to be used in next method to see if method should be run
canMoveColtoCol :: Board -> Bool
canMoveColtoCol (EOBoard board@(_,column,reserve))= (any isKing (getHeads column) && emptyColumn column) || any (toColumnsHelper column) (getHeads column)

-- this method is a combination of colToColMove and colToColKingMove, describes possible moves inside the columns. Ranked on purpose so the king to empty column comes first.
difColtoColMoves :: Board -> [Board]
difColtoColMoves (EOBoard board@(foundation, [], reserve)) = []
difColtoColMoves (EOBoard board@(foundation, column, reserve))
    | canMoveColtoCol (EOBoard board) && any (toColumnsHelper column) (getHeads column) = removeItem (EOBoard board) (kingsToEmpty ++ otherMoves)   -- all colToCol moves
    | any isKing (getHeads column) && emptyColumn column = removeItem (EOBoard board) kingsToEmpty                                                  -- all king to empty col from col moves
    | any (toColumnsHelper column) (getHeads column) = removeItem (EOBoard board) otherMoves                                                        -- all col to col moves from non king cards
    | otherwise = []
    where
        kingCards = getKingsAtColumnHead column
        kingsToEmpty = map (\x -> colToColKingMove x (EOBoard board)) kingCards                           -- maps colToColKingMove onto any kings at col heads
        otherMoves = map (\x -> colToColMove x (EOBoard board)) (getHeads column)                         -- maps colToColMove to all heads

{- This part is the combination of the difColumnMoves, difReservesMoves and colToColMoves. This method finds all 
   the possible moves in the game, with toFoundations called on each of the boards as instructed -}
findMoves :: Board -> [Board]
findMoves (EOBoard board@(foundation,column,reserves))
    | null (removeItem (EOBoard board) (map toFoundations allMoves)) = []
    | otherwise = removeItem (EOBoard board) (map toFoundations allMoves)      -- all the different moves, with toFoundations applied on them all
    where
        allMoves = [EOBoard board] ++ difColtoColMoves (EOBoard board) ++ difColumnMoves (EOBoard board) ++ difReservesMoves (EOBoard board)      --All moves

---------------------------------------------------------------Step 2: A FUNCTION TO CHOOSE THE NEXT MOVE---------------------------------------------------------------------------------------------
{- I have chosen to choose which move based on which one has the biggest number of cards in foundations. The moves are already ranked in order of importance in findMoves,
   with column to column coming first (nested in this is a king from a column to an empty column first), then reserves to tableau, then tableau to reserves. This therefore
   picks out of these the best move out of the moves with the highest amount of cards in foundations -}

-- finds the amount of cards in foundations for a given board
foundationSize :: Board -> Int
foundationSize (EOBoard board@([],_,_)) = 0
foundationSize (EOBoard board@(x:xs,column,reserve)) = length x + foundationSize (EOBoard (xs,column,reserve))

-- finds the board with the biggest number of cards in foundations
foundationBiggest :: [Board] -> Board
foundationBiggest = foldr1 (\x y -> if foundationSize x >= foundationSize y then x else y)

--The following 3 methods are to check if any of the next cards in any of the columns can move to foundations, if so, move the head to reserves then call to foundations
--This method checks if any of the second head cards in the tableau can move to the foundations
canSecondCardMove :: Board -> Bool
canSecondCardMove (EOBoard board@(foundation, [], [])) = False                -- if the columns are empty, there are no second cards to move
canSecondCardMove (EOBoard board@(foundation, [], _)) = False                 -- if the columns are empty, there are no second cards to move
canSecondCardMove (EOBoard board@(foundation, column, _))
    | any (toFoundationsHelper foundation) (getSecondHeads column) = True     -- checks if any of the second heads can move to the foundation
    | moveForSecondCard (EOBoard board) == EOBoard board = False
    | otherwise = False

-- This is a method to choose a card to move from column to the reserves if the card before the head is a s card of any in the foundations
moveForSecondCard :: Board -> Board
moveForSecondCard (EOBoard board@(foundation,[],[])) = EOBoard board
moveForSecondCard (EOBoard board@(foundation,[],_)) = EOBoard board
moveForSecondCard (EOBoard board@(foundation,x:xs,reserve))
    | isNothing (getSecondHead x) = EOBoard (newFound, x:newColumn, newReserve)
    | length x > 2 && toFoundationsHelper foundation (fromJust (getSecondHead x)) && canMoveToReserves (EOBoard board) = toFoundations (moveCardToReserves (head x) (toFoundations (EOBoard board)))  -- moves head card out the wau to reserves
    | length x > 2 && toFoundationsHelper foundation (fromJust (getSecondHead x)) && colToColMove (head x) (EOBoard board) /= EOBoard board = toFoundations (colToColMove (head x) (EOBoard board))   -- moves head card out the way to another column
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)    -- calls recursively if the second card in that column is not found to be able to move to foundations
    where
        (EOBoard (newFound, newColumn, newReserve)) = moveForSecondCard (EOBoard (foundation, xs, reserve))

{- finds heads from each what has a successor card behind it or a kind on an empty column. This is for the following method, to stop these cards being moved to
   and from the reserves infinitely. These cards should not be moved anyway in most cases -}
getImmovableHeads :: Column -> Deck
getImmovableHeads [] = []
getImmovableHeads (x:xs)
    | null x = getImmovableHeads xs                                                                   -- empty column, then continue search
    | not (null x) && length x < 2 && isKing (head x) = head x : getImmovableHeads xs                 -- if the king is on an empty column then he should not be moved
    | length x > 1 && sCard (head x) == fromJust (getSecondHead x) = head x : getImmovableHeads xs    -- if a card is in front of a successor, it should not be moved
    | otherwise = getImmovableHeads xs                                                                -- not found in this column, continue search

{- This is to be removed from all the moves in chooseMove, as this is the boards what move a card to the reserves when they have a 
   successor card behind them or if its a king in its own column. This is to be taken from the methods to choose from -}
findWrongCardsToRes :: Board -> [Board]
findWrongCardsToRes (EOBoard board@(foundation, [], reserve)) = [toFoundations (EOBoard board)]
findWrongCardsToRes (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = removeItem (EOBoard board) boards
    | otherwise = []
    where
        boards = map (\x -> moveCardToReserves x (EOBoard board)) (getImmovableHeads column)      -- all moves from the cards I dont want to move from the columns to the reserves


{- This method chooses the move next move. It first evaluates if there are any moves left, if not, nothing. 
   It then checks if any of the second cards can move, if so, moves the fornt card out the way and moves to the foundations. It then chooses the move with
   the most cards in the foundations, what are already in order in findMoves -}
chooseMove :: Board -> Maybe Board
chooseMove (EOBoard board@(foundation, column, reserve))
    | null allMoves = Nothing
    | canSecondCardMove (EOBoard board) && moveForSecondCard (EOBoard board) /= EOBoard board = Just (moveForSecondCard (EOBoard board))
    | bestAllMoves == EOBoard board = Nothing                                      -- if there is no move without toRemove, return nothing
    | otherwise = Just bestAllMoves                                                -- choose best moved, ranked from findMoves, with the biggest foundation size
    where
        allMoves = removeItem (EOBoard board) (findMoves (EOBoard board))          -- all of the moves
        toRemove = findWrongCardsToRes (EOBoard board)                             -- the moves to remove that are from columns to reserve that I dont want to choose from
        bestAllMoves = if null (removeItems toRemove allMoves) then EOBoard board else foundationBiggest (removeItems toRemove allMoves)  -- all moves without toRemove moves

-------------------------------------------------------Step 3: A FUNCTION TO PLAY A GAME OF EIGHT-OFF SOLITAIRE---------------------------------------------------------------------------------------
-- This is a function what should return true if the game has been won (i.e. if all cards have been moved from the tableau and reserves to the foundations)
haveWon :: Board -> Bool
haveWon (EOBoard board@(foundation, column, reserve))
    | null column && null reserve = True                    -- if column and reserves are empty, game is won
    | otherwise = False                                     -- otherwise, not won

--This function takes an initial Board as its argument and uses chooseMove to play the game to
--completion. The return value is the score, which is calculated by how many cards have been moved to
--the foundations – a successful game, in which all cards are moved to the foundations, will score 52.
playSolitaire :: Board -> Int
playSolitaire (EOBoard board@(foundation, column, reserve))
    | haveWon (EOBoard board) = 52                                               -- if the board has won then there must be 52 cards in the foundations
    | isNothing (chooseMove (EOBoard board)) = foundationSize (EOBoard board)    -- if there are no more moves in chooseMoves, then find score by finding the size in foundations
    | otherwise = playSolitaire (fromJust (chooseMove (EOBoard board)))          -- keep playing solitaire until no more moves

---------------------------------------------------------------STEP 4: A FUNCTION TO ANALYSE PERFORMANCE----------------------------------------------------------------------------------------------
--This gets the total score of x amount of games, i.e. how many cards went to foundations in all games
getTotalScore :: Int -> Int -> Int
getTotalScore seed numGames
    | numGames > 0 = playSolitaire (eODeal (seed + numGames)) + getTotalScore seed (numGames-1)
    | otherwise = 0

--This method gets the number of wins in x random games
getNumberWins :: Int -> Int -> Int
getNumberWins seed numGames
    | numGames > 0 && playSolitaire (eODeal (seed + numGames)) == 52 = 1 + getNumberWins seed (numGames-1)
    | otherwise = 0

--This method is a combination of the two above methods, uses totalScore to find the average score in x games
analyseEO :: Int -> Int -> (Int,Int)
analyseEO _ 0 = error "No amount of games specified"
analyseEO seed numGames = (wins, averageScore)
    where
        wins = getNumberWins seed numGames
        totalScore = getTotalScore seed numGames
        averageScore = totalScore `div` numGames

---------------------------------------------------------------------template.hs----------------------------------------------------------------------------------------------------------------------

-- Constants set:
studentName = "Jordan Pownall"
studentNumber = "190143099"
studentUsername = "acb19jp"

initialBoardDefined :: Board
initialBoardDefined = eODeal 118

secondBoardDefined :: Board
secondBoardDefined = sDeal 12345 

main :: IO()
main =
    do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

        let boards = findMoves board      -- show that findMoves is working
        putStrLn "***The possible next moves after that:"
        print boards

        let chosen = chooseMove board     -- show that chooseMove is working
        putStrLn "***The chosen move from that set:"
        print chosen

        putStrLn "***Now showing a full game"     -- display a full game
        score <- displayGame initialBoardDefined 0
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

        {- start comment marker - move this if appropriate
        putStrLn "***Now showing a full game for alternative solitaire"
        score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

        -}

{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
-}

displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
        then return "A WIN"
        else
        do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                    let (Just newBoard) = maybeBoard
                    displayGame newBoard (n+1)
                else
                do
                    let score = show (playSolitaire board)
                    return score