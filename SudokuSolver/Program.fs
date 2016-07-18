module SudokuSolver.Main

module Solver = Version1

open System
open System.IO

let printUsage () =
    printfn "Usage: %s <suduko file>" System.AppDomain.CurrentDomain.FriendlyName

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        GUI.showMainWindow ()
        0
    elif Array.exists (fun arg -> arg = "-h" || arg = "--help") args then
        printUsage ()
        0
    else
        for filepath in args do
            use fs = new FileStream(filepath, FileMode.Open, FileAccess.Read)
            use sr = new StreamReader(fs)

            printfn "%s" filepath
            while sr.Peek() <> -1 do
                let b = Solver.Board sr
                b.Show System.Console.Out

                printfn "vvvvvvvvvvv"

                let timer = System.Diagnostics.Stopwatch()
                timer.Start()

                if b.Solve ()
                then b.Show System.Console.Out
                else printfn "No solution"

                timer.Stop()
                printfn "Time: %A ms" timer.ElapsedMilliseconds
                printfn ""
        0