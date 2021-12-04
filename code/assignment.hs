{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import System.Random
import Data.Ord
import Debug.Trace
import Data.Maybe
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

shuffleBoards :: Int -> [Board] -> [Board]
shuffleBoards n boards = [board | (board,n) <- sortBy cmp (zip boards (randoms (mkStdGen n) :: [Int]))]

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
    | otherwise = EOBoard board                                     --otherwise do nothing
    where
        newBoard = foldr moveAceFoundations (EOBoard board) ((getHeads column) ++ reserve)


-- checks if the card can move (essentially if it is an Ace or if its predecessor of the head of a list in foundations)
toFoundationsHelper :: [Deck] -> Card -> Bool
toFoundationsHelper foundation c = isAce c || elem (pCard c) (getHeads foundation)

-- checks if any cards at the head of column and reserve meet helper i.e. can be moved to foundations or not
canMoveToFoundations :: Board -> Bool
canMoveToFoundations (EOBoard board@(foundation, [], [])) = False
canMoveToFoundations (EOBoard board@(foundation, column, [])) = any (toFoundationsHelper foundation) (getHeads column)
canMoveToFoundations (EOBoard board@(foundation, [], reserve)) = any (toFoundationsHelper foundation) (reserve)
canMoveToFoundations (EOBoard board@(foundation, column, reserve)) = any (toFoundationsHelper foundation) (getHeads column ++ reserve)

-- finds heads from each list to evaluate
getHeads :: [Deck] -> Deck
getHeads [] = []
getHeads (x:xs)
    | x == [] = getHeads xs
    | otherwise = head x : getHeads xs

-- removes the card from a stack if it is found at the head of any of the lists
removeHead :: Card -> [Deck] -> [Deck]
removeHead c [] = []
removeHead c [a] = []
removeHead c (x:xs)
    | c == head x = removeItem c x : xs                    -- if c is at the head of this list, delete
    | otherwise = x:removeHead c xs               -- if not, repeat process for rest of the list until found

-- this function checks if the ace card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation and removes it from its current position
moveAceFoundations :: Card -> Board -> Board
moveAceFoundations c (EOBoard board@(foundation, column, reserve))
    | isAce c = EOBoard ([c]:foundation, removeHead c column, removeItem c reserve)   --if the card is an ace it is added to foundations (empty list)
    | otherwise = moveCardFoundations c (EOBoard board)         --this way the function can be called every time in case theres an ace, if not will check for rest of cards

-- this function checks if the card (c) is present in any of the reserves or if it is the head of
-- the tableau columns, if so adds to the foundation it belongs to and removes it from its current position
moveCardFoundations :: Card -> Board -> Board
moveCardFoundations c (EOBoard ([], column, reserve)) = EOBoard ([], column, reserve)
moveCardFoundations c (EOBoard board@(x:xs, column, reserve))
    | head x == pCard c = EOBoard ((c:x):xs, removeHead c column, removeItem c reserve) -- checks if head of each foundation is the pCard of c
    | otherwise = EOBoard (x:newFound, newColumn, newReserve)    -- calls recursively if the card is not found to be able to move from current column
    where
        (EOBoard (newFound, newColumn, newReserve)) = moveCardFoundations c (EOBoard (xs, column, reserve))

--------------------------------------------------------------------------PART 2----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------Step 1: A FUNCTION TO FIND ALL POSSIBLE MOVES FOR EIGHT-OFF -----------------------------------------------------------------------------------
-- This part uses functions to find the possible moves from the columns (tableau) to the reserves 
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

-- this method finds the Board array from possible moves to the reserves
difReservesMoves :: Board -> [Board]
difReservesMoves (EOBoard board@(foundation, [], reserve)) = [toFoundations (EOBoard board)]
difReservesMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToReserves (EOBoard board) = removeItem (EOBoard board) boards
    | otherwise = []
    where
        boards = map (\x -> moveCardToReserves x (EOBoard board)) (getHeads column)

-- This part uses functions to find the possible moves from the reserves  to the columns (tableau)
-- checks if a successor card is the head of any of the lists in the tableau
toColumnsHelper :: [Deck] -> Card -> Bool
toColumnsHelper column c = (sCard c) `elem` (getHeads column)

-- checks if a card can be moved to the tableau
canMoveToColumn :: Board -> Bool
canMoveToColumn (EOBoard (_,column,reserve))
    | any (toColumnsHelper column) reserve = True
    | otherwise = False

-- moves a card to the tableau and deletes it from the reserves
moveCardToColumn :: Card -> Board -> Board
moveCardToColumn c (EOBoard board@(foundation, [], reserve)) = EOBoard board   --if the tableau is empty then just return board as you should be able to move all cards to column then
moveCardToColumn c (EOBoard board@(foundation, x:xs, reserve))
    | head x == sCard c = EOBoard (foundation, (c:x):xs, removeItem c reserve) -- checks if head of a column is the sCard of c, if so add c
    | x == [] = EOBoard (foundation, (c:x):xs, removeItem c reserve)           -- checks if head of a column is empty, if so add c
    | otherwise = EOBoard (newFound, x:newColumn, newReserve)                  -- calls recursively if the sCard is not found at the head of that column
    where
        EOBoard (newFound, newColumn, newReserve) = moveCardToColumn c (EOBoard (foundation, xs, reserve))

-- this method finds the Board array from possible moves from columns to the reserves
difColumnMoves :: Board -> [Board]
difColumnMoves (EOBoard board@(foundation, [], reserve)) = []
difColumnMoves (EOBoard board@(foundation, column, reserve))
    | canMoveToColumn (EOBoard board) = removeItem (EOBoard board) boards
    | otherwise = []
    where
        boards = map (\x -> moveCardToColumn x (EOBoard board)) reserve

--This method is for filtering, used in many functions
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- This part is the combination of the difColumnMoves and difReservesMoves. 
-- This method finds all the possible moves in the game, with toFoundations called on each of the boards as instructed,
-- if there are no more moves returns an empty set, otherwise shuffles the boards and returns that
-- shuffleBoard is used to stop the same card switching back and forth with a column
findMoves :: Board -> [Board]
findMoves (EOBoard board@(foundation,column,reserves))
    | removeItem (EOBoard board) (map toFoundations ([(EOBoard board)] ++ difReservesMoves (EOBoard board) ++ difColumnMoves (EOBoard board))) == [] = []
    | otherwise = shuffleBoards 1234 (removeItem (EOBoard board) (map toFoundations ([(EOBoard board)] ++ difReservesMoves (EOBoard board) ++ difColumnMoves (EOBoard board))))

---------------------------------------------------------------Step 2: A FUNCTION TO CHOOSE THE NEXT MOVE---------------------------------------------------------------------------------------------
--I have chosen to choose which move based on which one has the biggest number of cards in foundations.

-- finds the amount of cards in foundations for a given board
foundationSize :: Board -> Int
foundationSize (EOBoard board@([],_,_)) = 0
foundationSize (EOBoard board@(x:xs,column,reserve)) = length x + foundationSize (EOBoard (xs,column,reserve))

-- finds the board with the biggest number of cards in foundations
foundationBiggest :: [Board] -> Board
foundationBiggest = foldr1 (\x y -> if foundationSize x >= foundationSize y then x else y)

-- finds the amount of cards in foundations for a given board
reserveSize :: Board -> Int
reserveSize (EOBoard board@(_,_,[])) = 0
reserveSize (EOBoard board@(foundation,column,reserve)) = length reserve

-- Finds the board with the smallest number of cards in reserves
reserveSmallest :: [Board] -> Board
reserveSmallest = foldr1 (\x y -> if reserveSize x <= reserveSize y then x else y)

-- Chooses which board by finding the board what has the biggest foundation with the smallest reserve
combined :: [Board] -> Board
combined = foldr1 (\x y -> if (foundationSize x - reserveSize x) >= (foundationSize y - reserveSize y) then x else y)

--This method chooses the move dependant on the above method
chooseMove :: Board -> Maybe Board
chooseMove (EOBoard board@(foundation, column, reserve))
    | findMoves (EOBoard board) == [] = Nothing
    | otherwise = Just toChoose
    where
        toChoose = combined (findMoves (EOBoard board))

-------------------------------------------------------Step 3: A FUNCTION TO PLAY A GAME OF EIGHT-OFF SOLITAIRE---------------------------------------------------------------------------------------
haveWon :: Board -> Bool
haveWon (EOBoard board@(foundation, column, reserve))
    | column == [] && reserve == [] = True
    | otherwise = False

--This function takes an initial Board as its argument and uses chooseMove to play the game to
--completion. The return value is the score, which is calculated by how many cards have been moved to
--the foundations – a successful game, in which all cards are moved to the foundations, will score 52.
playSolitaire :: Board -> Int
playSolitaire (EOBoard board@(foundation, column, reserve))
    | haveWon (EOBoard board) = 52
    | chooseMove (EOBoard board) == Nothing = foundationSize (EOBoard board)
    | otherwise = playSolitaire (fromJust (chooseMove (EOBoard board)))

---------------------------------------------------------------------template.hs----------------------------------------------------------------------------------------------------------------------

{- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
-}
-- Constants that YOU must set:
studentName = "Jordan Pownall"
studentNumber = "190143099"
studentUsername = "acb19jp"

initialBoardDefined = eODeal 12345    {- replace XXX with the name of the constant that you defined
                                                                in step 3 of part 1 -}
{-secondBoardDefined = SBoard (Foundation, Column, Stock)  replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
    do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

        {- Move the start comment marker below to the appropriate position.
        If you have completed ALL the tasks for the assignment, you can
        remove the comments from the main function entirely.
        DO NOT try to submit/run non-functional code - you will receive 0 marks
        for ALL your code if you do, even if *some* of your code is correct.
        -}



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
{- start comment marker - move this if appropriate

        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

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
        DO NOT CHANGE THIS CODE other than aligning indentation with your own.
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