namespace DMBSim

type GridPosition =
    { x : int
      y : int  }

module GridPosition =
    
    let create (x: int, y: int) : GridPosition =
        { x = x; y = y; }
        
    let leftOf (pos: GridPosition) : GridPosition =
        { pos with x = pos.x - 1 }
        
    let rightOf (pos: GridPosition) : GridPosition =
        { pos with x = pos.x + 1 }

    let below (pos: GridPosition) : GridPosition =
        { pos with y = pos.y - 1 }
        
    let above (pos: GridPosition) : GridPosition =
        { pos with y = pos.y + 1 }