  {- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
  -- Constants that YOU must set:
  studentName = "Your Name Here"
  studentNumber = "Your Student Number"
  studentUsername = "Your Student Username"

  initialBoardDefined = XXX {- replace XXX with the name of the constant that you defined
                               in step 3 of part 1 -}
  secondBoardDefined = YYY {- replace YYY with the constant defined in step 5 of part 1,
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

      {- start comment marker - move this if appropriate

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
