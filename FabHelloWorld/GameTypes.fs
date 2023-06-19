namespace FabHelloWorld

open Xamarin.Forms
open Fabulous.XamarinForms
open System

open type View



open System

type BoardDifficulty = Easy | Medium | Hard 

type SquareValue = 
    Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | None
    member this.ToInt() =
        match this with
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | None -> -1
    member this.MatchInt(input) =
        match input with
        | 0 -> Zero 
        | 1 -> One
        | 2 -> Two 
        | 3 -> Three
        | 4 -> Four
        | 5 -> Five
        | 6 -> Six
        | 7 -> Seven   
        | 8 -> Eight
        | 9 -> Nine
        | _ -> None


type NoteValues = {noteValues: int list}

type GameSquare = 
    {squareValue : SquareValue; noteValue: NoteValues; isVisible: bool; posInPeer: int}

type PeerSquare = 
    {peerSquares : GameSquare list; positionOnGame: int}

(* General idea is the board is a 9x9 board of 'peer squares' numbered 1-9 starting
   with 1 in the upper left, 3 in the top right, 4 is middle left, etc. 
 *)
type GameBoard = 
    {peerSquares: PeerSquare list; difficulty: BoardDifficulty}


module BoardHelpers = 

    let randomValue(): SquareValue = 
        let squareValue: SquareValue = None
        let rand = new Random()
        rand.Next(10) |> squareValue.MatchInt


    let shuffleR (r : Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())





    let getOtherPeerPos (curPeerPos: int)
                        (isColCheck: bool) = 
        let returnList = 
            if isColCheck then 
                if curPeerPos % 3 = 0 then 
                    List.filter (fun x -> x = curPeerPos) [ 0; 3; 6]
                elif curPeerPos % 3 = 1 then
                    List.filter (fun x -> x = curPeerPos) [ 1; 4; 7]
                else
                    List.filter (fun x -> x = curPeerPos) [ 2; 5; 8]
            else
                if curPeerPos < 3 then 
                    List.filter (fun x -> x = curPeerPos) [ 0; 1; 2]
                elif curPeerPos > 2 && curPeerPos < 6 then
                    List.filter (fun x -> x = curPeerPos) [ 3; 4; 5]
                else 
                    List.filter (fun x -> x = curPeerPos) [ 6; 7; 8]
        
        returnList
    
    let slicePeerRows (posInPeer : int)
                      (otherPeer : PeerSquare) = 
        if posInPeer < 3 then
            otherPeer.peerSquares.[0..2]
        elif posInPeer > 2 &&  posInPeer < 6 then
            otherPeer.peerSquares.[3..5]
        else 
            otherPeer.peerSquares.[6..8]
        
    let slicePeerCols (posInPeer : int)
                      (otherPeer : PeerSquare) = 
        if posInPeer % 3 = 0 then
            [otherPeer.peerSquares.[0]; otherPeer.peerSquares.[3]; otherPeer.peerSquares.[6]]
        elif posInPeer % 3 = 1 then
            [otherPeer.peerSquares.[1]; otherPeer.peerSquares.[4]; otherPeer.peerSquares.[7]]
        else 
            [otherPeer.peerSquares.[2]; otherPeer.peerSquares.[5]; otherPeer.peerSquares.[8]]


    let peerValidCheck (curSquareVal: GameSquare) 
                       (curPeerSquare: PeerSquare) =
            let peerAsSV = List.map (fun x -> x.squareValue) curPeerSquare.peerSquares
            let existedInPeer = List.exists (fun x -> x = curSquareVal.squareValue) peerAsSV
            not existedInPeer


    let rowValidCheck (curSquareValue: GameSquare)
                      (otherPeerOne: PeerSquare)
                      (otherPeerTwo: PeerSquare) :
                      bool = 
            let peer1Row = slicePeerRows curSquareValue.posInPeer otherPeerOne
            let peer2Row = slicePeerRows curSquareValue.posInPeer otherPeerTwo

            let row1AsSV = List.map (fun x -> x.squareValue) peer1Row
            let existedInRow1 = List.exists (fun x -> x = curSquareValue.squareValue) row1AsSV 
            let row2AsSV = List.map (fun x -> x.squareValue) peer2Row
            let existedInRow2 = List.exists (fun x -> x = curSquareValue.squareValue) row2AsSV 
            not (existedInRow1 && existedInRow2)
        

    let colValidCheck (curSquareValue: GameSquare)
                      (otherPeerOne: PeerSquare)
                      (otherPeerTwo: PeerSquare) :
                      bool = 
            let peer1Col = slicePeerCols curSquareValue.posInPeer otherPeerOne
            let peer2Col = slicePeerCols curSquareValue.posInPeer otherPeerTwo

            let col1AsSV = List.map (fun x -> x.squareValue) peer1Col
            let existedInCol1 = List.exists (fun x -> x = curSquareValue.squareValue) col1AsSV 
            let col2AsSV = List.map (fun x -> x.squareValue) peer2Col
            let existedInCol2 = List.exists (fun x -> x = curSquareValue.squareValue) col2AsSV 
            not (existedInCol1 && existedInCol2)
        

    let gameValidCheck (curSquareValue: GameSquare)
                       (curPeerSquare: PeerSquare)
                       (gameBoard: GameBoard) :
                       bool = 
        let peerPosCol = getOtherPeerPos curPeerSquare.positionOnGame true
        let othPeerPosCol1 = peerPosCol.[0]
        let othPeerPosCol2 = peerPosCol.[1]
        let peerPosRow = getOtherPeerPos curPeerSquare.positionOnGame false
        let othPeerPosRow1 = peerPosRow.[0]
        let othPeerPosRow2 = peerPosRow.[1]
        let othPeerRow1 = gameBoard.peerSquares.[othPeerPosRow1]
        let othPeerRow2 =  gameBoard.peerSquares.[othPeerPosRow2]
        let othPeerCol1 = gameBoard.peerSquares.[othPeerPosCol1]
        let othPeerCol2 =  gameBoard.peerSquares.[othPeerPosCol2]
        let peerValid = peerValidCheck curSquareValue curPeerSquare // translate posInGame to posInPeer?
        let rowValid = rowValidCheck curSquareValue othPeerRow1 othPeerRow2
        let colValid = colValidCheck curSquareValue othPeerCol1 othPeerCol2
        rowValid && colValid && peerValid

    let generateAPeer (posInGame: int) (curGameBoard: GameBoard) : PeerSquare = 
        let mutable validValueList = [One; Two; Three; Four; Five; Six; Seven; Eight; Nine]
        let mutable newPeerSquares: GameSquare list = []
        let emptyNoteValue = {noteValues = []}
        while validValueList.Length > 0 do 
            let newValue = validValueList |> shuffleR (Random ()) |> Seq.head
            let newGameSquare = {squareValue = newValue; noteValue = emptyNoteValue; isVisible = true; posInPeer = validValueList.Length}
            while not (gameValidCheck newGameSquare {peerSquares = newPeerSquares; positionOnGame = posInGame} curGameBoard) do 
                let newValue = validValueList |> shuffleR (Random ()) |> Seq.head
                let newGameSquare = {squareValue = newValue; noteValue = emptyNoteValue; isVisible = true; posInPeer = validValueList.Length}
            
            newPeerSquares <- newPeerSquares @ [newGameSquare]
            validValueList <- List.except [newValue] validValueList
        {peerSquares = newPeerSquares; positionOnGame = 0}
    
    
    //let populateGameBoard = printf "to implement" // Fill in peer 1, 5, and 9, then solve the others?

    let printPeer (peer: PeerSquare) = 
        let peerAsSV = List.map (fun x -> x.squareValue) peer.peerSquares
        for sv in peerAsSV do
            printf "%A" (sv.ToInt())
            printf " " 
            if (List.findIndex (fun x -> x = sv) peerAsSV) % 3 = 2 then
                printfn ""

    let sliceRowFromPeers (sortedPeerList: PeerSquare list)
                startingIndex endingIndex = 
        sortedPeerList.[0].peerSquares.[startingIndex..endingIndex] @ 
        sortedPeerList.[1].peerSquares.[startingIndex..endingIndex] @ 
        sortedPeerList.[2].peerSquares.[startingIndex..endingIndex]  
    
    let printGameRow (fullRow: GameSquare list) = 
        for gs in fullRow do
            printf "%A" (gs.squareValue.ToInt())
            printf " "
        printfn "" 

    let printPeerRow (peerList: PeerSquare list) = 
        let sortedPeerList = List.sortBy (fun x -> x.positionOnGame) peerList
        let fullRow1 = sliceRowFromPeers  sortedPeerList 0 2
        printGameRow fullRow1 
        let fullRow2 = sliceRowFromPeers  sortedPeerList 3 5
        printGameRow fullRow2 
        let fullRow3 = sliceRowFromPeers  sortedPeerList 6 8
        printGameRow fullRow3

    let printGame (gameBoard: GameBoard) = 
        gameBoard.peerSquares[0..2] |> printPeerRow
        gameBoard.peerSquares[3..5] |> printPeerRow
        gameBoard.peerSquares[6..8] |> printPeerRow

    //let determineStartingSquares = printf "to implement"

    //let startTimer = printf "to implement"

let peer1 = BoardHelpers.generateAPeer()
let peer2 = BoardHelpers.generateAPeer()
let peer3 = BoardHelpers.generateAPeer()
let peer4 = BoardHelpers.generateAPeer()
let peer5 = BoardHelpers.generateAPeer()
let peer6 = BoardHelpers.generateAPeer()
let peer7 = BoardHelpers.generateAPeer()
let peer8 = BoardHelpers.generateAPeer()
let peer9 = BoardHelpers.generateAPeer()

BoardHelpers.printPeerRow [peer1; peer2; peer3]
printfn "-----------------"
BoardHelpers.printPeerRow [peer4; peer5; peer6]
printfn "-----------------"
BoardHelpers.printPeerRow [peer7; peer8; peer9]
