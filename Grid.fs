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
    
    type Msg =
        | Step
        | MoveChem of GridPosition * GridPosition * Chemical
        | AddChem of  GridPosition * Chemical
        | RemoveChem of GridPosition * Chemical
        | ImportProcedure of string
        | EditProcedure of string
        | ClearGrid
        | EditDest of string
        | EditPos of string
        // | PrintDroplets

    let update (msg:Msg) (grid:GridModel) : GridModel =
        match msg with
        | Step -> GridModel.handleProcedure (grid) 
        | MoveChem (pos,dest,chem) -> GridModel.moveChem (pos,dest,chem) (grid)
        | AddChem (dest,chem) -> GridModel.setDroplet (dest,GridModel.addChem(GridModel.getDroplet dest grid,chem)) (grid)
        | RemoveChem (dest,chem) -> GridModel.setDroplet (dest,GridModel.removeChem(GridModel.getDroplet dest grid,chem)) (grid)
        | ImportProcedure (path) -> GridModel.ImportProcedure (path) (grid)
        | EditProcedure (input) -> GridModel.EditProcedure (input) (grid)
        | ClearGrid -> GridModel.constructBlank(10, 10)
        | EditDest (dest) -> {grid with SelectedDest = GridModel.stringToGP dest}
        | EditPos (pos) -> {grid with SelectedPos = GridModel.stringToGP pos}
        // | PrintDroplets -> printfn "%A" grid.Droplets
        //                    grid

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
                    //Button.onClick ((fun _ -> EditProcedure ("AD")  |> dispatch))
                    Button.onClick ((fun _ -> AddChem (grid.SelectedDest,grid.Chem)  |> dispatch))
                    Button.content ""
                    
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
                
                
                
            ]
        ] |> generalize

    let private electrodeView (grid: GridModel) (dispatch : Msg -> unit) :IView =
        UniformGrid.create [
            UniformGrid.zIndex 0
            UniformGrid.width 600.0
            UniformGrid.height 400.0
            UniformGrid.dock Dock.Top
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
            StackPanel.width 800.
            StackPanel.height 400.
            StackPanel.orientation Orientation.Horizontal
            StackPanel.dock Dock.Top
            StackPanel.dock Dock.Right
            
            StackPanel.children[
                StackPanel.create[
                    StackPanel.width 800.
                    StackPanel.height 500.
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
                                    let r = List.fold (fun acc (s,v)-> acc + (v/2.)) 0.0 chemlist
                                    Ellipse.create[
                                        
                                        Ellipse.top (float y)
                                        Ellipse.left (float x)
                                        Ellipse.width r
                                        Ellipse.height r
                                        Ellipse.fill "#ecf0f1"
                                    ]
                                
                            ]
                        ]
                        
                        Button.create [
                            Button.zIndex 0
                            Button.height 40.0
                            Button.width 600.0
                            Button.dock Dock.Bottom
                            Button.background "#d35400"
                            Button.onClick ((fun _ -> ImportProcedure (fullPath)  |> dispatch), SubPatchOptions.OnChangeOf grid.Procedure)
                            Button.content "Import Procedure"
                        ]
                        Button.create [
                            Button.zIndex 0
                            Button.height 40.0
                            Button.width 600.0
                            Button.dock Dock.Bottom
                            Button.background "#d35400"
                            Button.onClick ((fun _ -> ClearGrid  |> dispatch), SubPatchOptions.OnChangeOf grid.Droplets)
                            Button.content "Clear"
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