// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Solve

//read two ints from string, fail if invalid format
let read2ints (str:System.String) =
    let sp = str.Split([|' '|],System.StringSplitOptions.RemoveEmptyEntries)
    match sp with
        | [|v; w|] -> int v, int w
        | _ -> failwithf "Invalid string %s %A" str sp

//zip values and weights into a single list of (weight, value, density=v/w) tuples
let wvd data = List.map (fun (v,w) -> (w, v, double v / double w)) data

let evalProfit wvd result = List.zip wvd result |> List.map (fun ((w,v,d),x) -> x*v) |> List.sum

[<EntryPoint>]
let main argv = 
    match argv |> Array.toList with
        | [fileName]
        | fileName :: _ ->
            let lines = System.IO.File.ReadLines(fileName) |> Seq.toList
            match lines with 
                | header :: dataLines -> 
                    let items, capacity = read2ints header
                    let data = List.map read2ints dataLines |> wvd
                    //solving here
                    let (result,isOptimal) = solve items capacity data
                    //output result
                    printf "%d %d\n" (evalProfit data result) (System.Convert.ToInt32(isOptimal)) //System.Environment.NewLine //\n works on my machine(tm) when called from python
                    let resstr = System.String.Join(" ", result |> List.map string |> List.toArray)
                    printf "%s\n" resstr //System.Environment.NewLine
                    0
                | _ -> failwith "No header"
        | _ -> 
            printf "Usage: knapsack.exe filename"
            1    
