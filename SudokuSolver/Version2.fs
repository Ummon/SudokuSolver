module SudokuSolver.Version2

open System
open System.IO
open System.Collections.Generic

let printUsage progname = 
    printfn "Usage: %A <filename>" progname

type Pos = 
    { i: int; j: int }
    member this.Next : Pos option = 
        match this with
        | { i = 8; j = 8 } -> None
        | { i = i; j = 8 } -> Some { i = i + 1; j = 0 }
        | { i = _; j = j } -> Some { this with j = j + 1 }

let zoneRange c = 
    match c with
    | 0 | 1 | 2 -> [0 .. 2]
    | 3 | 4 | 5 -> [3 .. 5]
    | _ -> [6 .. 8]

// All possible positions.
let AllPos = seq {
    for i in 0 .. 8 do
       for j in 0 .. 8 -> { i = i; j = j } }

type Board (values : seq<int>) =
    let size = 9
    let board = Array2D.create size size 0

    do
        Seq.take (size * size) values |> Seq.zip AllPos |> Seq.iter (fun ({ i = iVal; j = jVal}, value) -> 
            board.[iVal, jVal] <- value)

    let get pos = board.[pos.i, pos.j]
    let set pos value = board.[pos.i, pos.j] <- value

    let rec nextFree (pos : Pos) : Pos option =
        match pos.Next with
        | Some pos -> if get pos = 0 then Some pos else nextFree pos
        | _ -> None

    let isValid pos n =
        List.forall (fun j -> get { pos with j = j } <> n) [0 .. 8] && 
        List.forall (fun i -> get { pos with i = i } <> n) [0 .. 8] &&
        List.forall (fun (i, j) -> get { i = i; j = j } <> n) [ 
            for i' in zoneRange pos.i do
                for j' in zoneRange pos.j -> i', j' ]

    let validNumbers pos =
        [
            let valid = isValid pos
            for n in 1 .. 9 do
                if valid n then yield n ]

    let show (output : TextWriter) =
        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                if board.[i, j] = 0
                then output.Write '.' 
                else output.Write board.[i, j]
                if (j + 1) % 3 = 0 && j <> size - 1 then
                    output.Write '|'
            output.WriteLine ()
            if (i + 1) % 3 = 0 && i <> size - 1 then
                output.WriteLine "-----------"         


    let presolve () =
        let (|OnlyOneNumber|_|) (pos : Pos) =
            if get pos <> 0
            then None
            else
                let numbers = Array.create 10 false
                let nb = ref 0
                let add n = 
                    if not numbers.[n]
                    then
                        numbers.[n] <- true
                        nb := !nb + 1

                for i in 0 .. 8 do get { pos with i = i } |> add
                for j in 0 .. 8 do get { pos with j = j } |> add
                for i in zoneRange pos.i do
                    for j in zoneRange pos.j do
                        get { i = i; j = j } |> add

                match !nb with
                | 9 -> try Some (Array.findIndex not numbers) with _ -> None
                | 10 -> None
                | _ ->
                    // For all remaining numbers.
                    let remainingNumbers = Array.mapi (fun i p -> i, p) numbers 
                                            |> Array.fold (fun acc (i, p) -> if not p then i :: acc else acc) []

                    let rec findNumber numbers =
                        match numbers with
                        | [] -> None
                        | n :: tail ->
                            // If there is no other valid position, then the current is the only one.
                            if seq {
                                    for i in 0 .. 8 do
                                        let pos' = { pos with i = i }
                                        if i <> pos.i && get pos' = 0
                                        then yield not (isValid pos' n) } |> Seq.forall id ||
                               seq {
                                    for j in 0 .. 8 do
                                        let pos' = { pos with j = j }
                                        if j <> pos.j && get pos' = 0
                                        then yield not (isValid pos' n) } |> Seq.forall id ||
                               seq {
                                    for i in zoneRange pos.i do
                                        for j in zoneRange pos.j do
                                            let pos' = { i = i; j = j }
                                            if pos' <> pos && get pos' = 0
                                            then yield not (isValid pos' n) } |> Seq.forall id
                            then Some n
                            else findNumber tail

                    findNumber remainingNumbers

        while Seq.exists (fun pos -> 
            match pos with
            | OnlyOneNumber n -> set pos n; true
            | _ -> false) AllPos do ()

    new (input : TextReader) =
        Board (seq {
            while input.Peek () <> -1 do
                match char (input.Read ()) with
                | ' ' | '.' | '0' -> yield 0
                | a when Char.IsDigit a -> yield int (Char.GetNumericValue a)
                | _ -> () } |> Seq.take 81)

    member this.Show = show
    
    member this.Solve () =
        let rec solveFrom pos : bool = // Returns true if the solution is valid and complete.
            match nextFree pos with
            | Some pos' -> 
                if List.exists (fun n -> set pos' n; solveFrom pos') (validNumbers pos')
                then true
                else 
                    set pos' 0
                    false
            | _ -> true
        presolve ()
        solveFrom { i = 0; j = -1 }
