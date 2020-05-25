(*
enterprise-tic-tac-toe.fsx
An example of implementing "enterprise" tic-tac-toe in a functional way.
Related blog post: http://fsharpforfunandprofit.com/posts/enterprise-tic-tac-toe/
*)

open System

module TicTacToeDomain =

    type HorizPosition = Left | HCenter | Right
    type VertPosition = Top | VCenter | Bottom
    type CellPosition = HorizPosition * VertPosition 

    type Player = PlayerO | PlayerX

    type CellState = 
        | Played of Player 
        | Empty

    type Cell = {
        pos : CellPosition 
        state : CellState 
        }

    type PlayerXPos = PlayerXPos of CellPosition 
    type PlayerOPos = PlayerOPos of CellPosition 

    type ValidMovesForPlayerX = PlayerXPos list
    type ValidMovesForPlayerO = PlayerOPos list
        
    type MoveResult = 
        | PlayerXToMove of ValidMovesForPlayerX 
        | PlayerOToMove of ValidMovesForPlayerO 
        | GameWon of Player 
        | GameTied 

    type NewGame<'GameState> = 
        'GameState * MoveResult      
    type PlayerXMoves<'GameState> = 
        'GameState -> PlayerXPos -> 'GameState * MoveResult
    type PlayerOMoves<'GameState> = 
        'GameState -> PlayerOPos -> 'GameState * MoveResult

    type GetCells<'GameState> = 
        'GameState -> Cell list

    type TicTacToeAPI<'GameState>  = 
        {
        newGame : NewGame<'GameState>
        playerXMoves : PlayerXMoves<'GameState> 
        playerOMoves : PlayerOMoves<'GameState> 
        getCells : GetCells<'GameState>
        }

module TicTacToeImplementation =
    open TicTacToeDomain

    type GameState = {
        cells : Cell list
        }

    let allHorizPositions = [Left; HCenter; Right]
    
    let allVertPositions = [Top; VCenter; Bottom]

    type Line = Line of CellPosition list

    let linesToCheck = 
        let mkHLine v = Line [for h in allHorizPositions do yield (h,v)]
        let hLines= [for v in allVertPositions do yield mkHLine v] 

        let mkVLine h = Line [for v in allVertPositions do yield (h,v)]
        let vLines = [for h in allHorizPositions do yield mkVLine h] 

        let diagonalLine1 = Line [Left,Top; HCenter,VCenter; Right,Bottom]
        let diagonalLine2 = Line [Left,Bottom; HCenter,VCenter; Right,Top]

        [
        yield! hLines
        yield! vLines
        yield diagonalLine1 
        yield diagonalLine2 
        ]

    let getCells gameState = 
        gameState.cells 

    let getCell gameState posToFind = 
        gameState.cells 
        |> List.find (fun cell -> cell.pos = posToFind)

    let private updateCell newCell gameState =

        let substituteNewCell oldCell =
            if oldCell.pos = newCell.pos then
                newCell
            else 
                oldCell                 

        let newCells = gameState.cells |> List.map substituteNewCell 
        
        {gameState with cells = newCells }

    let private isGameWonBy player gameState = 
        
        let cellWasPlayedBy playerToCompare cell = 
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false

        let lineIsAllSamePlayer player (Line cellPosList) = 
            cellPosList 
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)

        linesToCheck
        |> List.exists (lineIsAllSamePlayer player)


    let private isGameTied gameState = 
        let cellWasPlayed cell = 
            match cell.state with
            | Played _ -> true
            | Empty -> false

        gameState.cells
        List.forall cellWasPlayed 

    let private remainingMovesForPlayer playerMove gameState = 

        let playableCell cell = 
            match cell.state with
            | Played player -> None
            | Empty -> Some (playerMove cell.pos)

        gameState.cells
        |> List.choose playableCell


    let newGame = 

        let allPositions = [
            for h in allHorizPositions do 
            for v in allVertPositions do 
                yield (h,v)
            ]

        let emptyCells = 
            allPositions 
            |> List.map (fun pos -> {pos = pos; state = Empty})
        
        let gameState = { cells=emptyCells }            

        let validMoves = 
            allPositions 
            |> List.map PlayerXPos

        gameState, PlayerXToMove validMoves

    let playerXMoved gameState (PlayerXPos cellPos) = 
        let newCell = {pos = cellPos; state = Played PlayerX}
        let newGameState = gameState |> updateCell newCell 
        
        if newGameState |> isGameWonBy PlayerX then
            newGameState, GameWon PlayerX
        elif newGameState |> isGameTied then
            newGameState, GameTied  
        else
            let remainingMoves = 
                newGameState |> remainingMovesForPlayer PlayerOPos
            newGameState, PlayerOToMove remainingMoves

    let playerOMoved gameState (PlayerOPos cellPos) = 
        let newCell = {pos = cellPos; state = Played PlayerO}
        let newGameState = gameState |> updateCell newCell 
        
        if newGameState |> isGameWonBy PlayerO then
            newGameState, GameWon PlayerO
        elif newGameState |> isGameTied then
            newGameState, GameTied 
        else
            let remainingMoves = 
                newGameState |> remainingMovesForPlayer PlayerXPos
            newGameState, PlayerXToMove remainingMoves

    let api = {
        newGame = newGame 
        playerXMoves = playerXMoved 
        playerOMoves = playerOMoved 
        getCells = getCells
        }

module ConsoleUi =
    open TicTacToeDomain
    
    type UserAction<'a> =
        | ContinuePlay of 'a
        | ExitGame

    let displayAvailableMoves moves = 
        moves
        |> List.iteri (fun i move -> 
            printfn "%i) %A" i move )

    let getMove moveIndex moves = 
        if moveIndex < List.length moves then
            let move = List.nth moves moveIndex 
            Some move
        else
            None

    let processMoveIndex inputStr gameState availableMoves makeMove processInputAgain = 
        match Int32.TryParse inputStr with
        | true,inputIndex ->
            match getMove inputIndex availableMoves with
            | Some move -> 
                let moveResult = makeMove gameState move 
                ContinuePlay moveResult
            | None ->

                printfn "...No move found for inputIndex %i. Try again" inputIndex 
 
                processInputAgain()
        | false, _ -> 

            printfn "...Please enter an int corresponding to a displayed move."             

            processInputAgain()

    let rec processInput gameState availableMoves makeMove = 

        let processInputAgain() = 
            processInput gameState availableMoves makeMove 

        printfn "Enter an int corresponding to a displayed move or q to quit:" 
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            ExitGame
        else
            processMoveIndex inputStr gameState availableMoves makeMove processInputAgain
            
    let displayCells cells = 
        let cellToStr cell = 
            match cell.state with
            | Empty -> "-"            
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"

        let printCells cells  = 
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
            |> printfn "|%s|"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Top) 
        let centerCells = 
            cells |> List.filter (fun cell -> snd cell.pos = VCenter) 
        let bottomCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Bottom) 
        
        printCells topCells
        printCells centerCells 
        printCells bottomCells 
        printfn ""   

    let rec askToPlayAgain api  = 
        printfn "Would you like to play again (y/n)?"             
        match Console.ReadLine() with
        | "y" -> 
            ContinuePlay api.newGame
        | "n" -> 
            ExitGame
        | _ -> askToPlayAgain api 

    let rec gameLoop api userAction = 
        printfn "\n------------------------------\n" 
        
        match userAction with
        | ExitGame -> 
            printfn "Exiting game."             
        | ContinuePlay (state,moveResult) -> 
            state |> api.getCells |> displayCells

            match moveResult with
            | GameTied -> 
                printfn "GAME OVER - Tie"             
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | GameWon player -> 
                printfn "GAME WON by %A" player            
                printfn ""             
                let nextUserAction = askToPlayAgain api 
                gameLoop api nextUserAction
            | PlayerOToMove availableMoves -> 
                printfn "Player O to move" 
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerOMoves
                gameLoop api newResult 
            | PlayerXToMove availableMoves -> 
                printfn "Player X to move" 
                displayAvailableMoves availableMoves
                let newResult = processInput state availableMoves api.playerXMoves
                gameLoop api newResult 

    let startGame api =
        let userAction = ContinuePlay api.newGame
        gameLoop api userAction 

module Logger = 
    open TicTacToeDomain
     
    let logXMove (PlayerXPos cellPos)= 
        printfn "X played %A" cellPos

    let logOMove (PlayerOPos cellPos)= 
        printfn "O played %A" cellPos

    let injectLogging api =

        let playerXMoves state move = 
            logXMove move 
            api.playerXMoves state move 

        let playerOMoves state move = 
            logOMove move 
            api.playerOMoves state move 

        { api with
            playerXMoves = playerXMoves
            playerOMoves = playerOMoves
            }

module ConsoleApplication = 

    let startGame() =
        let api = TicTacToeImplementation.api
        let loggedApi = Logger.injectLogging api
        ConsoleUi.startGame loggedApi 

ConsoleApplication.startGame() 
