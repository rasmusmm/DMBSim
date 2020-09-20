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
    let baseDirectory = __SOURCE_DIRECTORY__
    let baseDirectory' = Directory.GetParent(baseDirectory)
    let filePath = "DMBSim\commands.txt"
    let fullPath = Path.Combine(baseDirectory'.FullName, filePath)
    
    type Msg =
        | MoveChem of GridPosition * GridPosition * Chemical
        | AddChem of  GridPosition * Chemical
        | RemoveChem of GridPosition * Chemical
        | ImportProcedure of string
        // | PrintDroplets

    let update (msg:Msg) (grid:GridModel):GridModel =
        match msg with
        | MoveChem (pos,dest,chem) -> GridModel.moveChem (pos,dest,chem) (grid)
        | AddChem (dest,chem) -> GridModel.setDroplet (dest,GridModel.addChem(GridModel.getDroplet dest grid,chem)) (grid)
        | RemoveChem (dest,chem) -> GridModel.setDroplet (dest,GridModel.removeChem(GridModel.getDroplet dest grid,chem)) (grid)
        | ImportProcedure (path) -> GridModel.ImportProcedure (path) (grid)
        // | PrintDroplets -> printfn "%A" grid.Droplets
        //                    grid
    let view (grid: GridModel) (dispatch: Msg -> unit) : IView =
         DockPanel.create[
                DockPanel.zIndex 0
                DockPanel.children[
                    Canvas.create[
                        Canvas.zIndex 1
                        Canvas.children[
                            let dropletlist = List.map (GridModel.DropletValues) grid.Droplets
                            for (chemlist,x,y) in dropletlist do
                                let r = List.fold (fun acc (s,v)-> acc + v) 0.0 chemlist
                                Ellipse.create[
                                    Ellipse.top (float x)
                                    Ellipse.left (float y)
                                    Ellipse.width r
                                    Ellipse.height r
                                    Ellipse.fill "#ecf0f1"
                                ]
                        ]
                    ]
                    Button.create [
                        Button.dock Dock.Bottom
                        Button.background "#d35400"
                        Button.onClick ((fun _ -> ImportProcedure (fullPath)  |> dispatch), SubPatchOptions.OnChangeOf grid.Droplets)
                        Button.content "Import Procedure"
                    ]
                    // Button.create [
                    //     Button.dock Dock.Bottom
                    //     Button.background "#d35400"
                    //     Button.onClick ((fun _ ->  PrintDroplets |> dispatch), SubPatchOptions.OnChangeOf grid.Droplets)
                    //     Button.content "show droplet"
                    // ]
                    UniformGrid.create [
                    UniformGrid.zIndex 0
                    UniformGrid.columns grid.Width
                    UniformGrid.rows grid.Height
                    UniformGrid.children(
                        grid.Electrodes
                        |> Array2D.flati
                        |> Array.map (fun (x, y, electrode) ->
                            let electrodePosition = { x = x; y = y }
                            
                            Button.create [
                                match electrode.Activation with
                                | false ->
                                    (*
                                    yield Button.onClick ((fun _ -> AddChem (electrodePosition,("test",10.1)) |> dispatch), SubPatchOptions.OnChangeOf electrodePosition)
                                    *)
                                    yield Button.background "gray"
                                | true ->
                                    (*
                                    yield Button.onClick ((fun _ -> RemoveChem (electrodePosition,("test",10.1))  |> dispatch), SubPatchOptions.OnChangeOf electrodePosition)
                                    *)
                                    yield Button.background "green"
                                
                            ] |> generalize                     
                        )
                        |> Array.toList
                    )
                    ]
                ]
            ] |> generalize
        

(*

        Canvas.create [
            Canvas.children[



              
                
                

*)