module TableModel

open System

type Column = {
    Name: string
    Alias: string option
}
with 
    member this.ColumnDisplay() =
            match this.Alias with
            | Some a when not (String.IsNullOrWhiteSpace a) -> sprintf "%s AS %s" this.Name a
            | _ -> this.Name

    member this.ColumnRef() =
        match this.Alias with
        | Some a when not (String.IsNullOrWhiteSpace a) -> a
        | _ -> this.Name

type Table = {
    Name: string
    Alias: string option
    Columns: Column list // (column name, optional alias)
}
with
    // get the name with which to reference the table in SQL (alias if provided, otherwise name)
    member this.TableRef() =
        match this.Alias with
        | Some a when not (String.IsNullOrWhiteSpace a) -> a
        | _ -> this.Name

    // how this table should be displayed in the FROM clause (name AS alias if alias provided, otherwise just name)
    member this.TableDisplay() =
        match this.Alias with
        | Some a when not (String.IsNullOrWhiteSpace a) -> sprintf "%s AS %s" this.Name a
        | _ -> this.Name

    // how columns should be displayed in the SELECT clause (name AS alias if alias provided, otherwise just name)
    member this.ColumnsDisplay() =
        match this.Columns with
        | [] -> ""
        | cols ->
            cols
            |> List.map (fun c ->
                match c.Alias with
                | Some a when not (String.IsNullOrWhiteSpace a) -> sprintf "%s AS %s" c.Name a
                | _ -> c.Name)
            |> String.concat ", "

    override this.ToString() =
        sprintf "Table: %s; Columns: %s" (this.TableDisplay()) (this.ColumnsDisplay())

