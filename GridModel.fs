namespace DMBSim

open System.IO

type Chemical = (string * float)

type Electrode = {
    ChemList : Chemical list
    Activation : bool
}
type Droplet ={
    ChemList : Chemical list
    Pos : GridPosition
}
type GridModel = { 
    Width : int
    Height : int
    Electrodes : Electrode[,]
    Droplets : Droplet list
}

module GridModel =

    let setDroplet (pos : GridPosition, droplet:Droplet) (grid:GridModel) : GridModel =
        if List.exists (fun d -> d.Pos = pos) grid.Droplets then
            printfn "droplet does exist, updating"
            { grid with Droplets = (List.map (fun d -> if d.Pos = pos then droplet else d ) grid.Droplets)}
            
        else
            printfn "droplet with pos doesnt exist, creating one"
            { grid with Droplets = grid.Droplets @ [droplet]}
    let getDroplet (pos: GridPosition) (grid: GridModel)  : Droplet =
        if List.exists (fun d -> d.Pos = pos) grid.Droplets then
            List.find (fun droplet -> droplet.Pos = pos) grid.Droplets
        else
            printfn "#1"
            {ChemList = [];Pos = pos;}

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
          Droplets = []  
        } 

    let DropletValues (droplet : Droplet) =
        (droplet.ChemList,droplet.Pos.x, droplet.Pos.y)
    
    let stringToGP (input : string) : GridPosition =
        let poslist = input.Split [|','|] |> Array.toList
        let newGP = {GridPosition.x = poslist.[0] |> int ;GridPosition.y = poslist.[1] |> int}
        newGP

    let stringToChemical (input : string) : Chemical =
        let chemList = input.Split [|','|] |> Array.toList
        let newChem = (chemList.[0],chemList.[1] |> float)
        newChem

    let RemoveEmptyChem (chemList : Chemical list) : Chemical list =
        List.filter (fun (s,v) ->  v > 0.0 ) chemList
    
    let addChem (droplet : Droplet, chemical : Chemical) : Droplet =
        if List.exists (fun (s,v) -> s = fst chemical) droplet.ChemList then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,v + (snd chemical)) else (s,v)) droplet.ChemList
            {droplet with ChemList = newChemList}
        else
            printfn "#2"
            {droplet with ChemList = droplet.ChemList @ [chemical]}
    
    let removeChem (droplet : Droplet, chemical : Chemical) : Droplet =
        if (List.exists (fun (s,v) -> s = fst chemical) droplet.ChemList && List.exists (fun (s,v) -> v >= snd chemical) droplet.ChemList)  then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,(v - (snd chemical))) else (s,v)) droplet.ChemList |>  RemoveEmptyChem
            {droplet with ChemList = newChemList}
        else
            failwith "Failure removing chemical from droplet. Confirm that droplet contains chemical in larger quantity than you are trying to remove."

    let checkIfNeighbour (pos: GridPosition, dest: GridPosition):bool =
        ((pos.x = dest.x+1 || pos.x = dest.x-1 || pos.x = dest.x) && (pos.y = dest.y+1 || pos.y = dest.y-1 || pos.y=dest.y))
         
     

    let rec moveChem (pos : GridPosition, dest : GridPosition, chemical : Chemical) (grid : GridModel) : GridModel =
        match pos,dest with
        | pos,dest when pos.x>dest.x -> moveChem ({pos with x = pos.x-1},dest,chemical) (grid)
        | pos,dest when pos.x<dest.x -> moveChem ({pos with x = pos.x+1},dest,chemical) (grid)
        | pos,dest when pos.y>dest.y -> moveChem ({pos with y = pos.y-1},dest,chemical) (grid)
        | pos,dest when pos.y>dest.y -> moveChem ({pos with y = pos.y+1},dest,chemical) (grid)
        | pos,dest when checkIfNeighbour (pos,dest) -> let posdroplet = removeChem ((getDroplet (dest) (grid)),chemical)
                                                       let destdroplet = addChem ((getDroplet (dest) (grid)),chemical)
                                                       setDroplet (dest,posdroplet) (grid)
                                                       |> setDroplet (dest,destdroplet)
        | _ -> failwith "error moving chemical."
    
    

    let rec handleProcedure (input:string list list) (grid:GridModel) : GridModel =
        printfn "Handling Procedure"
        printfn "%A" grid.Droplets
        match input with
        | [cmd;pos;dest;chem]::sl when cmd = "MV" -> moveChem (stringToGP pos , stringToGP dest, stringToChemical chem) (grid) |> handleProcedure (sl)
        | [cmd;dest;chem]::sl when cmd = "AD" -> let droplet = addChem ((getDroplet (stringToGP dest) (grid)),stringToChemical chem)
                                                 setDroplet ((stringToGP dest),droplet) (grid) |> handleProcedure (sl)
        | [cmd;dest;chem]::sl when cmd = "RM" -> let droplet = removeChem ((getDroplet ((stringToGP dest)) (grid)),stringToChemical chem)
                                                 setDroplet ((stringToGP dest,droplet)) (grid) |> handleProcedure (sl)
        | _ -> grid
        
        
    let ImportProcedure (path:string) (grid:GridModel) : GridModel =
        printfn "Import Initiated"
        let lines = File.ReadAllLines(path) |> Seq.toList |> List.map (fun x -> Array.toList (x.Split [|' '|]) ) 
        handleProcedure lines grid