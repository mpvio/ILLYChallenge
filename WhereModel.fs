module WhereModel
open System.Globalization
open TableModel

type WhereValueType =
    | StringValue of string
    | NumberValue of float
    | BooleanValue of bool
    | ListValue of WhereValueType list // for IN
    | RangeValue of (float * float) // for BETWEEN
    | AnyValue of obj

type Operator =
    | Equals
    | NotEquals
    | GreaterThan
    | LessThan
    | GreaterThanOrEqual
    | LessThanOrEqual
    | Between
    | In
    | Like
    override this.ToString() =
        match this with
        | Equals -> "="
        | NotEquals -> "<>"
        | GreaterThan -> ">"
        | LessThan -> "<"
        | GreaterThanOrEqual -> ">="
        | LessThanOrEqual -> "<="
        | Between -> "BETWEEN"
        | In -> "IN"
        | Like -> "LIKE"

let mapStringToOperator (s: string) =
    match s.ToLowerInvariant() with
    | "=" -> Operator.Equals
    | "<>" -> Operator.NotEquals
    | ">" -> Operator.GreaterThan
    | "<" -> Operator.LessThan
    | ">=" -> Operator.GreaterThanOrEqual
    | "<=" -> Operator.LessThanOrEqual
    | "between" -> Operator.Between
    | "in" -> Operator.In
    | "like" -> Operator.Like
    | other ->
        printfn "unrecognised input so defaulting to Equals."
        Operator.Equals

let mapOperatorToValueType (op: Operator) =
    match op with
    | Operator.Between -> RangeValue (0.0, 0.0)
    | Operator.In -> ListValue []
    | Operator.Equals -> AnyValue null
    | Operator.NotEquals -> AnyValue null
    | Operator.GreaterThan -> NumberValue 0.0
    | Operator.LessThan -> NumberValue 0.0
    | Operator.GreaterThanOrEqual -> NumberValue 0.0
    | Operator.LessThanOrEqual -> NumberValue 0.0
    | Operator.Like -> StringValue ""

type Where = {
    Table: Table
    Column: Column
    Operator: Operator
    Value: WhereValueType
}
with 
    member this.ValueStringSimple() =
        match this.Value with
        | NumberValue n -> n.ToString(CultureInfo.InvariantCulture)
        | StringValue s -> sprintf "'%s'" s
        | BooleanValue b -> if b then "TRUE" else "FALSE"
        | AnyValue o -> 
            match o with
            | :? float as f -> f.ToString(CultureInfo.InvariantCulture)
            | :? string as s -> sprintf "'%s'" s
            | :? bool as b -> if b then "TRUE" else "FALSE"
            | _ -> o.ToString()
        | ListValue _ | RangeValue _ -> "(...)" // not simple values

    member this.ValueString() =
        match this.Value with
        | StringValue _ | NumberValue _ | BooleanValue _ | AnyValue _ -> this.ValueStringSimple()
        | ListValue vs ->
            let vals = 
                vs 
                |> List.map (function
                    | StringValue _ | NumberValue _ | BooleanValue _ | AnyValue _ -> this.ValueStringSimple()
                    | _ -> failwith "Invalid value type for IN operator")
                |> String.concat ", "
            sprintf "(%s)" vals
        | RangeValue (min, max) ->
            sprintf "%f AND %f" min max

    override this.ToString() =
        sprintf "%s %s %s" (this.Column.ColumnRef()) (this.Operator.ToString()) (this.ValueString())