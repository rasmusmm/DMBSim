namespace DMBSim

open System.IO

type Chemical = (string * float)

type Electrode = {
    ChemList : Chemical list
    Activation : bool
}
type Droplet ={
    ChemList : Chemical list
    Radius : float
    Pos : GridPosition
}
type GridModel = { 
    Width : int
    Height : int
    Electrodes : Electrode[,]
    Droplets : Droplet list
}

module GridModel =
    
    let setElectrode (pos : GridPosition, electrode:Electrode) (grid:GridModel) : GridModel =
        { grid with Electrodes = Array2D.set grid.Electrodes pos.x pos.y electrode  }

    let getElectrode (pos: GridPosition) (grid: GridModel)  : Electrode =
        Array2D.get grid.Electrodes pos.x pos.y

    let private tryGetElectrode (grid: GridModel, pos: GridPosition) : Electrode option =
        let xInRange = pos.x >= 0 && pos.x < grid.Width
        let yInRange = pos.y >= 0 && pos.y < grid.Height
        if (xInRange && yInRange) then
            try Some (Array2D.get grid.Electrodes pos.x pos.y)
            with _ -> None
        else None

    let constructBlank (width: int, height: int) : GridModel =
        { Width = width
          Height = height
          Electrodes = Array2D.create width height { ChemList = [] ; Activation = false}
          Droplets = [
                    {ChemList = [("test",1.1)];
                    Radius = 1000.1;
                    Pos = {x=10;y=10}}]  } 

    let DropletValues (droplet : Droplet) =
        (droplet.ChemList,droplet.Pos.x, droplet.Pos.y, droplet.Radius)
    
    let RemoveEmptyChem (chemList : Chemical list) : Chemical list =
        List.filter (fun (s,v) ->  v > 0.0 ) chemList
    
    let addChem (electrode : Electrode, chemical : Chemical) : Electrode =
        if List.exists (fun (s,v) -> s = fst chemical) electrode.ChemList then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,v + (snd chemical)) else (s,v)) electrode.ChemList
            {electrode with ChemList = newChemList}
        else
            {electrode with ChemList = electrode.ChemList @ [chemical]}

    let removeChem (electrode : Electrode, chemical : Chemical) : Electrode =
        if (List.exists (fun (s,v) -> s = fst chemical) electrode.ChemList && List.exists (fun (s,v) -> v >= snd chemical) electrode.ChemList)  then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,(v - (snd chemical))) else (s,v)) electrode.ChemList |>  RemoveEmptyChem
            
            {electrode with ChemList = newChemList}
        else
            failwith "Failure removing chemical from electrode. Confirm that electrode contains chemical in larger quantity than you are trying to remove."

    let checkIfNeighbour (pos: GridPosition, dest: GridPosition):bool =
        ((pos.x = dest.x+1 || pos.x = dest.x-1 || pos.x = dest.x) && (pos.y = dest.y+1 || pos.y = dest.y-1 || pos.y=dest.y))
         
     

    let rec moveChem (pos : GridPosition, dest : GridPosition, chem : Chemical) (grid : GridModel) : GridModel =
        if checkIfNeighbour (pos,dest) then
            setElectrode (dest,(addChem ((getElectrode dest grid),chem))) grid |>
            setElectrode (pos, (removeChem ((getElectrode pos grid),chem)))
        else 
        match pos,dest with
        | pos,dest when pos.x>dest.x -> moveChem ({pos with x = pos.x-1},dest,chem) (grid)
        | pos,dest when pos.x<dest.x -> moveChem ({pos with x = pos.x+1},dest,chem) (grid)
        | pos,dest when pos.y>dest.y -> moveChem ({pos with y = pos.y-1},dest,chem) (grid)
        | pos,dest when pos.y>dest.y -> moveChem ({pos with y = pos.y+1},dest,chem) (grid)    
        | _ -> failwith "error moving chemical."
    
    let stringToGP (input : string) : GridPosition =
        let poslist = input.Split [|','|] |> Array.toList
        let newGP = {GridPosition.x = poslist.[0] |> int ;GridPosition.y = poslist.[1] |> int}
        newGP

    let stringToChemical (input : string) : Chemical =
        let chemList = input.Split [|','|] |> Array.toList
        let newChem = (chemList.[0],chemList.[1] |> float)
        newChem

    let rec handleProcedure (input:string list list) (grid:GridModel) : GridModel =
        printfn "Handling Procedure"
        match input with
        | [cmd;pos;dest;chem]::sl when cmd = "MV" -> moveChem (stringToGP pos , stringToGP dest, stringToChemical chem) (grid) |> handleProcedure (sl)
        | [cmd;dest;chem]::sl when cmd = "AD" -> setElectrode ((stringToGP dest,(addChem ((getElectrode ((stringToGP dest)) (grid)),stringToChemical chem)))) (grid) |> handleProcedure (sl)
        | [cmd;dest;chem]::sl when cmd = "RM" -> setElectrode ((stringToGP dest,(removeChem ((getElectrode ((stringToGP dest)) (grid)),stringToChemical chem)))) (grid) |> handleProcedure (sl)
        | _ -> grid
        
        
    let ImportProcedure (path:string) (grid:GridModel) : GridModel =
        printfn "Import Initiated"
        let lines = File.ReadAllLines(path) |> Seq.toList |> List.map (fun x -> Array.toList (x.Split [|' '|]) ) 
        handleProcedure lines grid