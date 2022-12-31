module Game where

    import Data.List.Split
    import Data.Char
    import Board

    -- either 'f' or 'e' and a position
    data Command = Command Char (Int, Int)

    -- makes the move the user wanted
        -- input:
        -- move - flag or reveal and a position
        -- current board
        -- ouptput: 
        -- board after move is made
    makeMove :: Command -> Board -> Board
    makeMove (Command 'f' pos) board    | checkForMine board (Command 'f' pos) = flag board pos FlagBomb
                                        | otherwise = flag board pos FlagEmpty
    makeMove (Command 'r' pos) board = openMine board pos
    makeMove _ board = board

    -- changes status of board to true for lost game
        -- input:
        -- board
        -- ouptut: 
        -- board with changed status
    explodedBomb :: Board -> Board
    explodedBomb (Board size cells mines _) = Board size cells mines True

    -- checks if the current move will explode a bomb
        -- input: 
        -- current move
        -- current board
        -- output: 
        -- true for bomb will explode false for no bomb at that location
    isGameFinished :: Command -> Board -> Bool
    isGameFinished (Command 'r' pos) board = checkForMine board (Command 'r' pos) 
    isGameFinished (Command _ _) _ = False

    -- checks if there is a mine at the location of the position in the command
        -- input:
        -- current board
        -- current command
        -- output: 
        -- True if there is a bomb false if not
    checkForMine :: Board -> Command -> Bool
    checkForMine (Board _ cells _ _) (Command _ (x, y))  | cells !! ((x*10)+y) == Bomb ||  cells !! ((x*10)+y) == FlagBomb = True
                                            | otherwise = False

    -- converts each cell to a character for printing the game
        -- input: 
        -- cell
        -- output: 
        -- character equivalent
    convertCells:: Cell -> Char
    convertCells Empty = '-'
    convertCells Bomb = '-'
    convertCells FlagBomb = 'f'
    convertCells FlagEmpty = 'e'
    convertCells (Neighbours num) = intToDigit num

     -- converts each cell to a character for printing the game
        -- input: 
        -- cell
        -- output: 
        -- character equivalent
    printingLostGame:: Cell -> Char
    printingLostGame Bomb = 'x'
    printingLostGame _ = '-'