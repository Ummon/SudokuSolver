module SudokuSolver.Main

module Solver = Version1

open System
open System.IO

[<EntryPoint>]
let main argv = 
    use fs = new FileStream ("../../../sudokus/mm_22.txt", FileMode.Open, FileAccess.Read)
    use sr = new StreamReader (fs)

    while sr.Peek () <> -1 do
        let b = Solver.Board sr
        b.Show System.Console.Out

        printfn "vvvvvvvvvvv"

        let timer = System.Diagnostics.Stopwatch ()
        timer.Start ()

        if b.Solve () 
        then b.Show System.Console.Out
        else printfn "No solution"

        timer.Stop ()
        printfn "Time: %A ms" timer.ElapsedMilliseconds
    0