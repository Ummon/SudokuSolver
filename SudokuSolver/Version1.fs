module SudokuSolver.Version1

open System
open System.Threading
open System.IO
open System.Collections.Generic

[<Struct>]
type Pos (i: int, j: int) =
    member this.I = i
    member this.J = j
    member this.Next : Pos option =
        match this.I, this.J with
        | 8, 8 -> None
        | _, 8 -> Some (Pos(this.I + 1, 0))
        | _ -> Some (Pos(this.I, this.J + 1))
    override this.ToString() =
        sprintf "Pos(i = %i, j = %i)" this.I this.J

let inline zoneRange c =
    match c with
    | 0 | 1 | 2 -> [ 0 .. 2 ]
    | 3 | 4 | 5 -> [ 3 .. 5 ]
    | _ -> [ 6 .. 8 ]

// All possible positions.
let allPos = [
    for i in 0 .. 8 do
       for j in 0 .. 8 -> Pos(i, j) ]

let size = 9

type Board (values: int [,]) =
    let board = Array2D.create size size 0

    do
        Array2D.blit values 0 0 board 0 0 size size

    let get (pos: Pos) = board.[pos.I, pos.J]
    let set (pos: Pos) (value: int) = board.[pos.I, pos.J] <- value

    let rec nextFree (pos: Pos) : Pos option =
        match pos.Next with
        | Some pos -> if get pos = 0 then Some pos else nextFree pos
        | _ -> None

    let isValid (pos: Pos) (n: int) =
        List.forall (fun j -> j = pos.J || board.[pos.I, j] <> n) [ 0 .. 8 ] &&
        List.forall (fun i -> i = pos.I || board.[i, pos.J] <> n) [ 0 .. 8 ] &&
        List.forall (fun (i, j) -> (i = pos.I && j = pos.J ) || board.[i, j] <> n) [
            for i' in zoneRange pos.I do
                for j' in zoneRange pos.J -> i', j' ]

    let validNumbers pos = seq {
        for n = 1 to 9 do
            if isValid pos n then yield n }

    let show (output: TextWriter) =
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                if board.[i, j] = 0 then output.Write '.'
                else output.Write board.[i, j]
                if (j + 1) % 3 = 0 && j <> size - 1 then
                    output.Write '|'
            output.WriteLine()
            if (i + 1) % 3 = 0 && i <> size - 1 then
                output.WriteLine "-----------"

    let presolve () =
        let (|OnlyOneNumber|_|) (pos : Pos) =
            if get pos <> 0 then
                None
            else
                let numbers = Array.create 10 false
                let mutable nb = 0
                let add n =
                    if not numbers.[n] then
                        numbers.[n] <- true
                        nb <- nb + 1

                for i = 0 to 8 do get (Pos(i, pos.J)) |> add
                for j = 0 to 8 do get (Pos(pos.I, j)) |> add
                for i in zoneRange pos.I do
                    for j in zoneRange pos.J do
                        get (Pos(i, j)) |> add

                match nb with
                | 9 -> Array.tryFindIndex not numbers
                | 10 -> None
                | _ ->
                    // For all remaining numbers.
                    let remainingNumbers = Array.mapi(fun i p -> i, p) numbers
                                            |> Array.fold(fun acc (i, p) -> if not p then i :: acc else acc) []

                    let rec findNumber numbers =
                        match numbers with
                        | [] -> None
                        | n :: tail ->
                            // If there is no other valid position, then the current is the only one.
                            if seq {
                                    for i = 0 to 8 do
                                        let pos' = Pos(i, pos.J)
                                        if i <> pos.I && get pos' = 0
                                        then yield not (isValid pos' n) } |> Seq.forall id ||
                               seq {
                                    for j = 0 to 8 do
                                        let pos' = Pos(pos.I, j)
                                        if j <> pos.J && get pos' = 0
                                        then yield not (isValid pos' n) } |> Seq.forall id ||
                               seq {
                                    for i in zoneRange pos.I do
                                        for j in zoneRange pos.J do
                                            let pos' = Pos(i, j)
                                            if pos' <> pos && get pos' = 0
                                            then yield not (isValid pos' n) } |> Seq.forall id
                            then Some n
                            else findNumber tail

                    findNumber remainingNumbers

        while allPos |> List.exists (fun pos ->
            match pos with
            | OnlyOneNumber n -> set pos n; true
            | _ -> false) do ()

    new (input: TextReader) =
        let matrix = Array2D.create size size 0
        [ while input.Peek () <> -1 do
                    match char (input.Read()) with
                    | ' ' | '.' | '0' -> yield 0
                    | a when Char.IsDigit a -> yield int (Char.GetNumericValue a)
                    | _ -> () ]
        |> List.take (size * size)
        |> List.zip allPos
        |> List.iter(fun (pos, value) -> matrix.[pos.I, pos.J] <- value)
        Board(matrix)

    member this.Show = show

    member this.Values : int [,] =
        Array2D.copy board

    member this.SolveAsync (token: CancellationToken) : Async<bool> =
        async {
            let rec solveFrom pos : bool = // Returns true if the solution is valid and complete.
                if token.IsCancellationRequested then
                    false
                else
                    match nextFree pos with
                    | Some pos' ->
                        if validNumbers pos' |> Seq.exists (fun n -> set pos' n; solveFrom pos') then
                            true
                        else
                            set pos' 0
                            false
                    | _ -> true
            let valid =
                allPos |> List.forall (
                    fun p ->
                        let n = get p
                        if n = 0 then true else isValid p n)
            return
                if not valid then
                    false
                else
                    presolve ()
                    solveFrom (Pos(0, -1)) }

    member this.Solve () : bool =
        let cancellation = new CancellationTokenSource()
        this.SolveAsync(cancellation.Token) |> Async.RunSynchronously