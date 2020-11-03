namespace DMBSim

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI

module Grid =
    open Avalonia.FuncUI.Types
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.Controls.Shapes
    open System
    open System.IO
    open Avalonia.Controls
    open Avalonia.Layout
    let baseDirectory = __SOURCE_DIRECTORY__
    let baseDirectory' = Directory.GetParent(baseDirectory)
    let filePath = "DMBSim\commands.txt"
    let fullPath = Path.Combine(baseDirectory'.FullName, filePath)

    let dropletString (dl:Droplet list):string =
        List.fold (fun acc d -> let chemlist = List.fold (fun acc1 (s,v)-> acc1 + sprintf "Name: %s, Volume: %f \n" s v) "" d.ChemList
                                acc + sprintf "Position: %d,%d Chemicals:\n %s" d.Pos.x d.Pos.y chemlist
        ) "" dl
    type Msg =
        | Step
        | ImportProcedure of string
        | EditProcedure of string
        | ClearGrid
        | EditDest of string
        | EditPos of string
        | EditChem of string
        | RemoveStep

    let update (msg:Msg) (grid:GridModel) : GridModel =
        match msg with
        | Step -> GridModel.handleProcedure (grid)
        | RemoveStep -> GridModel.removeProcStep (grid)
        | ImportProcedure (path) -> GridModel.ImportProcedure (path) (grid)
        | EditProcedure (input) -> GridModel.EditProcedure (input) (grid)
        | ClearGrid -> GridModel.constructBlank(10, 10)
        | EditDest (dest) -> {grid with SelectedDest = GridModel.stringToGP dest}
        | EditPos (pos) -> {grid with SelectedPos = GridModel.stringToGP pos}
        | EditChem (chemical) -> {grid with Chem = GridModel.stringToChemical chemical}

    let private rightsideControlsView (grid: GridModel) (dispatch : Msg -> unit) :IView =
        StackPanel.create [
            StackPanel.zIndex 0
            StackPanel.dock Dock.Right
            StackPanel.children [
                TextBlock.create [
                    TextBlock.width 200.
                    TextBlock.height 100.
                    TextBlock.margin (horizontal = 3.0,vertical = 10.0)
                    TextBlock.fontSize 16.0
                    TextBlock.verticalAlignment VerticalAlignment.Top
                    TextBlock.text (grid.PlainProcedure)
                ]
                Button.create [
                    Button.onClick ((fun _ -> EditProcedure ("AD")  |> dispatch))
                    Button.content "Add"
                    
                ]
                Button.create [
                    Button.onClick ((fun _ -> EditProcedure ("RM")  |> dispatch))
                    Button.content "Remove"
                    
                ]
                Button.create [
                    Button.onClick ((fun _ -> EditProcedure ("SP")  |> dispatch))
                    Button.content "Split"
                    
                ]
                Button.create [
                    Button.onClick ((fun _ -> EditProcedure ("MV")  |> dispatch))
                    Button.content "Move"
                    
                ]
                Button.create [
                    Button.onClick ((fun _ -> RemoveStep  |> dispatch))
                    Button.content "Remove step"
                    
                ]
                TextBox.create [
                    TextBox.width 200.
                    TextBox.height 50.
                    TextBox.verticalAlignment VerticalAlignment.Bottom
                    TextBox.watermark "Insert start position as x,y"
                    TextBox.text (GridModel.GPToString grid.SelectedPos)
                    TextBox.onTextChanged (EditPos >> dispatch)
                ]
                TextBox.create [
                    TextBox.width 200.
                    TextBox.height 50.
                    TextBox.verticalAlignment VerticalAlignment.Bottom
                    TextBox.watermark "Insert destination position as x,y"
                    TextBox.text (GridModel.GPToString grid.SelectedDest)
                    TextBox.onTextChanged (EditDest >> dispatch)
                            
                ]
                TextBox.create [
                    TextBox.width 200.
                    TextBox.height 50.
                    TextBox.verticalAlignment VerticalAlignment.Bottom
                    TextBox.watermark "Insert Chemical as name,volume"
                    TextBox.text (GridModel.ChemicalToString grid.Chem)
                    TextBox.onTextChanged (EditChem >> dispatch)
                            
                ]
                
                
                
                
            ]
        ] |> generalize

    let private electrodeView (grid: GridModel) (dispatch : Msg -> unit) :IView =
        UniformGrid.create [
            UniformGrid.zIndex 0
            UniformGrid.width 600.0
            UniformGrid.height 400.0
            UniformGrid.dock Dock.Top
            UniformGrid.dock Dock.Left
            UniformGrid.columns grid.Width
            UniformGrid.rows grid.Height
            UniformGrid.children(
                grid.Electrodes
                |> Array2D.flati
                |> Array.map (fun (x, y, electrode) ->
                    let electrodePosition = { x = x; y = y }
                    
                    Button.create [
                        match electrode.Activation with
                        | false -> yield Button.background "gray"
                        | true -> yield Button.background "green"
                        
                    ] |> generalize                     
                )
                |> Array.toList
            )
        ] |> generalize    
    let view (grid: GridModel) (dispatch: Msg -> unit) : IView =
        StackPanel.create [
            StackPanel.zIndex 0
            StackPanel.width 1200.
            StackPanel.height 400.
            StackPanel.orientation Orientation.Horizontal
            
            StackPanel.children[
                TextBlock.create [
                            TextBlock.dock Dock.Left
                            TextBlock.width 200.
                            TextBlock.height 400.
                            TextBlock.margin (horizontal = 3.0,vertical = 10.0)
                            TextBlock.fontSize 16.0
                            TextBlock.verticalAlignment VerticalAlignment.Top
                            TextBlock.text ("Droplets : \n"+dropletString (grid.Droplets))
                ]
                
                StackPanel.create[
                    StackPanel.width 800.
                    StackPanel.height 400.
                    StackPanel.children[
                        
                        Canvas.create[
                            Canvas.zIndex 1
                            Canvas.width 600.0
                            Canvas.height 400.0
                            Canvas.dock Dock.Top
                            Canvas.dock Dock.Left
                            Canvas.children[
                                electrodeView grid dispatch
                                let dropletlist = List.map (GridModel.DropletValues) grid.Droplets
                                for (chemlist,x,y) in dropletlist do
                                    //we use the formula for a cylinder since the droplet is pressed between two planes
                                    // V = Pi * r^2 * h
                                    //We have V and h and use this information to find r
                                    let V = (List.fold (fun acc (s,v)-> acc + (v)) 0.0 chemlist)
                                    let r =  Math.Sqrt ((V/(Math.PI * 0.3)))
                                    let d = r*2.
                                    let cellMidWidth = (600./(grid.Width |> float))
                                    let cellMidHeight = (400./(grid.Height |> float))
                                    Ellipse.create[
                                        Ellipse.top ((((float y)*cellMidHeight))-(0.5*cellMidHeight)-r)
                                        Ellipse.left ((((float x)*cellMidWidth))-(0.5*cellMidWidth)-r)
                                        Ellipse.width d
                                        Ellipse.height d
                                        Ellipse.fill "#ecf0f1"
                                    ]
                                
                            ]
                        ]
                        
                        Button.create [
                            Button.zIndex 0
                            Button.height 40.0
                            Button.width 600.0
                            Button.dock Dock.Bottom
                            Button.background "#16a085"
                            Button.onClick ((fun _ -> ImportProcedure (fullPath)  |> dispatch), SubPatchOptions.OnChangeOf grid.Procedure)
                            Button.content "Import Procedure"
                        ]
                        Button.create [
                            Button.zIndex 0
                            Button.height 40.0
                            Button.width 600.0
                            Button.dock Dock.Bottom
                            Button.background "#16a085"
                            Button.onClick ((fun _ -> ClearGrid  |> dispatch), SubPatchOptions.OnChangeOf grid.Droplets)
                            Button.content "Clear"
                        ]
                        TextBlock.create [
                            TextBlock.width 600.
                            TextBlock.text (grid.ErrorMessage)
                        ]
                        
                    ]
                ]
                rightsideControlsView grid dispatch
                
            ]
        ] |> generalize
         
        

(*

        Canvas.create [
            Canvas.children[



              
                
                

*)