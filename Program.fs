open TableController
open JoinController
open WhereController
open SelectController
open System

// get tables
let tables = addTables [] ()

// get selected columns
let (selectClause, updatedTables) = createSelect tables

// get joins
let joins = getJoins updatedTables ()

// get where clauses
let wheres, operators = getWheres updatedTables ()

// put everything together
let selectPart = 
    sprintf "SELECT %s FROM %s" 
        selectClause 
        (updatedTables.Head.TableDisplay()) // only need first table in FROM clause, rest will be in JOINs

let joinPart =
    if joins.Length > 0 then
        joins 
        |> List.map (fun j -> j.ToString()) 
        |> String.concat " "
    else
        ""

let wherePart =
    if wheres.Length > 0 then
        let whereStrings = 
            wheres 
            |> List.map (fun w -> w.ToString())
        // combine with logical operators
        let combinedWhere = 
            List.zip whereStrings (operators @ [""]) // add empty operator to enable zipping for all where clauses
            |> List.map (fun (w, op) -> if op <> "" then sprintf "%s %s" w op else w)
            |> String.concat " "
        sprintf "WHERE %s" combinedWhere
    else
        ""

let fullSql = 
    [selectPart; joinPart; wherePart]
    |> List.filter (fun part -> not (String.IsNullOrWhiteSpace part))
    |> String.concat " "

printfn "\nGenerated SQL Query:\n%s" fullSql

// string outputs
//for t in tables do
//    printfn "%s" (t.ToString())

//for ut in updatedTables do
//    printfn "Updated %s" (ut.ToString())

//for j in joins do
//    printfn "%s" (j.ToString())

//for w in wheres do
//    printfn "%s" (w.ToString())

//for o in operators do
//    printfn "Logical operator: %s" o