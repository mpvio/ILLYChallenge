module WhereController

open TableModel
open TableController
open WhereModel
open HelperFunctions
open System
open System.Globalization

let getOperator () : Operator =
    let ops =
        [ Equals
          NotEquals
          GreaterThan
          LessThan
          GreaterThanOrEqual
          LessThanOrEqual
          Between
          In
          Like ]

    ops |> List.iteri (fun i op -> printfn "%d) %s" (i + 1) (op.ToString()))
    let idx = chooseIndex (List.length ops) "Enter number of operator: "
    List.item idx ops

let getValueForOperator (op: Operator) : WhereValueType =
    match op with
    | Between -> 
        let lower = promptUser "Enter lower bound for BETWEEN: " |> float
        let upper = promptUser "Enter upper bound for BETWEEN: " |> float
        RangeValue (lower, upper)
    | In -> 
        let rec getList acc =
            let input = promptUser "Enter value for IN (leave blank to finish): "
            if String.IsNullOrWhiteSpace(input) then
                List.rev acc
            else
                getList (StringValue input :: acc)
        ListValue (getList [])
    | Equals | NotEquals -> 
        let input = promptUser "Enter value for operator: "
        AnyValue (input :> obj)
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> 
        let input = promptUser "Enter numeric value for operator: "
        match Double.TryParse(input, CultureInfo.InvariantCulture) with
        | true, v -> NumberValue v
        | _ ->
            printfn "Invalid number. Defaulting to 0."
            NumberValue 0.0
    | Like -> 
        let input = promptUser "Enter pattern for LIKE (use % as wildcard): "
        StringValue input

let private createWhereFromTables (tables: Table list): Where =
    let table = chooseTable tables "Select table to query:"
    let column = chooseColumn table "Select column to filter on:"
    let operator = getOperator ()
    let value = getValueForOperator operator
    
    // create and return where clause
    {
        Table = table
        Column = column
        Operator = operator
        Value = value
    }

let private andOrOr () : string =
    let options = ["AND"; "OR"]
    options |> List.iteri (fun i opt -> printfn "%d) %s" (i + 1) opt)
    let idx = chooseIndex (List.length options) "Enter number of logical operator to combine WHERE clauses: "
    List.item idx options

let rec private whereMaker (acc: Where list) (logicalOperators: string list) (tables: Table list) (): (Where list * string list) =
    if yesNo "Do you want to add a WHERE clause?" then
        let logicalOp = if acc.Length > 0 then Some (andOrOr ()) else None
        let j = createWhereFromTables tables
        if logicalOp.IsSome then
            printfn "Combining with previous WHERE clauses using %s." logicalOp.Value
            whereMaker (j :: acc) (logicalOp.Value :: logicalOperators) (tables: Table list) ()
        else
             whereMaker (j :: acc) logicalOperators (tables: Table list) ()
    else
        List.rev acc, List.rev logicalOperators

let getWheres (tables: Table list) () : (Where list * string list) =
    whereMaker [] [] tables ()

