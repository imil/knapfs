module Greedy

//recursive greedy solver
//note: result is a REVERSED (to keep f# list operations simple) list of {1|0}
//params: taken - current result; weight - weight of current result; capacity - capacity of knapsack; wvd - list of (weight, value, density) tuples
//note: density and value don't get used in this method
let rec greedySolveRec taken weight capacity wvd =
     match wvd with
        | [] -> taken //end of available items
        | (w,v,d) :: tail -> //some items remain
            if weight + w > capacity //check if next item fits
            then greedySolveRec (0::taken) weight capacity tail  //if not, skip
            else greedySolveRec (1::taken) (weight+w) capacity tail //if yes, take and increase accumulated weight

//init with empty starting result and run recursive solver
let greedySolve nItems capacity wvd =
    (greedySolveRec [] 0 capacity wvd |> List.rev), false