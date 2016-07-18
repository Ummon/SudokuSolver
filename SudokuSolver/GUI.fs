module SudokuSolver.GUI

open System
open System.Threading
open System.Collections.ObjectModel

open Eto
open Eto.Forms
open Eto.Drawing

module Solver = Version1

type DigitBox() as this =
    inherit Panel()
    let unselectedBgColor = Colors.LightSkyBlue
    let selectedBgColor = Colors.Blue
    let digitChanged = new Event<unit>()
    let mutable manuallyAssigned = false
    let mutable value = 0
    let label =
        new Label(
            BackgroundColor = Colors.White,
            Text = " ",
            TextAlignment = TextAlignment.Center,
            VerticalAlignment = VerticalAlignment.Center,
            Font = new Font("Monospace", 18.f))

    do
        this.Padding <- Padding(2)
        this.BackgroundColor <- unselectedBgColor
        this.Content <- label

    member this.Selected
        with set (value: bool) =
            this.BackgroundColor <- if value then selectedBgColor else unselectedBgColor

    member this.ManuallyAssigned
        with get () : bool = manuallyAssigned
        and private set (value: bool) =
            label.BackgroundColor <- if value then Colors.Orange else Colors.White
            manuallyAssigned <- value

    member this.DigitChanged = digitChanged.Publish

    member this.Value = value

    member this.SetValue(v, manuallyAssigned) =
        if manuallyAssigned || not this.ManuallyAssigned || v = 0 then
            let changed = value <> v
            value <- v
            label.Text <- if value > 0 && value <= 9 then string value else " "
            this.ManuallyAssigned <- v <> 0 && manuallyAssigned
            if manuallyAssigned && changed then
                digitChanged.Trigger()

type Grid() =
    inherit TableLayout()

type MainForm() as this =
    inherit Form()
    do
        let mutable currentAsync : Async<Solver.Board> option = None
        let mutable cancellation = new CancellationTokenSource()

        this.Title <- "Sudoku Solver - gburri"
        this.Size <- Size(400, 400)
        let digitBoxes = Array2D.init 9 9 (fun _ _ -> new DigitBox())

        let clearComputedDigits () =
            digitBoxes |> Array2D.iter
                (fun d ->
                    if not d.ManuallyAssigned then
                        d.SetValue(0, false))

        let computeSolution () =
            cancellation.Cancel()
            cancellation.Dispose()
            cancellation <- new CancellationTokenSource()
            let board =
                Solver.Board(
                    digitBoxes
                    |> Array2D.map (fun d -> if d.ManuallyAssigned then d.Value else 0))

            clearComputedDigits ()

            let token = cancellation.Token
            async {
                let! result = board.SolveAsync(token)
                if not token.IsCancellationRequested then
                    Application.Instance.Invoke(
                        fun () ->
                            if result then
                                Array2D.iteri (fun i j v -> digitBoxes.[i, j].SetValue(v, false)) board.Values
                            else
                                clearComputedDigits ()) }
            |> Async.Start

        let mutable currentDigitBox = digitBoxes.[0,0]
        digitBoxes.[0,0].Selected <- true
        this.KeyDown.Add(
            fun e ->
                if e.Key = Keys.Backspace || e.Key = Keys.Delete then
                    currentDigitBox.SetValue(0, true)
                else
                    match Int32.TryParse(e.KeyChar.ToString()) with
                    | (true, digit) when digit >= 0 && digit <= 9 -> currentDigitBox.SetValue(digit, true)
                    | _ -> ())

        let setCurrentDigitBox db =
            if db <> currentDigitBox then
                currentDigitBox.Selected <- false
                db.Selected <- true
                currentDigitBox <- db

        digitBoxes
        |> Array2D.iter
            (fun digitBox ->
                digitBox.MouseDown.Add(fun _ -> setCurrentDigitBox digitBox)
                digitBox.DigitChanged.Add (fun _ -> computeSolution ()))

        let gridLayout = new Grid()

        for i = 0 to 8 do
            // Horizontal separations.
            if i = 3 || i = 6 then
                let rowSeparation = new TableRow()
                gridLayout.Rows.Add(rowSeparation)
                for j = 0 to 10 do
                    rowSeparation.Cells.Add(new TableCell(new Panel(BackgroundColor = Colors.Black)))
            let row = new TableRow(ScaleHeight = true)
            gridLayout.Rows.Add(row)
            for j = 0 to 8 do
                // Vertical separations.
                if j = 3 || j = 6 then
                    row.Cells.Add(new TableCell(new Panel(BackgroundColor = Colors.Black)))
                row.Cells.Add(new TableCell(digitBoxes.[i, j], true))

        this.Content <- gridLayout
        computeSolution ()

let showMainWindow () =
    use app = new Application()
    use form = new MainForm()
    form.Show()
    app.Run(form) |> ignore