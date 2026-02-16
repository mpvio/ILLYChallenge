module TableController

open HelperFunctions
open TableModel
open System

// TABLE CREATION FUNCTIONS
// get table name and optional alias
let rec private getTableName (): string =
    let tableName = promptUser "Enter table name: "
    if String.IsNullOrWhiteSpace(tableName) then
        printf "Please enter a table name"
        getTableName ()
    else
    tableName

// get column names
// for each column ask for an optional alias
let rec private getColumns (tableName: string) (columns: string list): string list =
    let columnName = promptUser (sprintf "Enter column name from table '%s' (leave blank to skip): " tableName)
    if String.IsNullOrWhiteSpace(columnName) then
        List.rev columns // get columns in the order they were entered
    else
        getColumns tableName (tableName + "." + columnName :: columns)

// DISPLAY FUNCTIONS
// display columns in a numbered list
let displayColumns (cols: Column list) =
    cols
    |> List.iteri (fun i c ->
        let aliasText =
            match c.Alias with
            | Some a when not (String.IsNullOrWhiteSpace a) -> sprintf " AS %s" a
            | _ -> ""
        printfn "%d) %s%s" (i + 1) c.Name aliasText)

// display tables in a numbered list
let displayTables (tables: Table list) =
    tables
    |> List.iteri (fun i t -> printfn "%d) %s" (i + 1) (t.TableDisplay()))

// CHOOSE FUNCTIONS
// choosing functions for joins and where clauses
let chooseTable (tables: Table list) (promptText: string): Table =
    displayTables tables
    let idx = chooseIndex (List.length tables) promptText
    List.item idx tables

let chooseColumn (table: Table) (promptText: string): Column =
    match table.Columns with
    | [] ->
        failwithf "No columns in table '%s'!" table.Name
    | cols ->
        displayColumns cols
        let idx = chooseIndex (List.length cols) promptText
        List.item idx cols

// for select query
// also allows user to define aliases for columns, which are added to the table record
let chooseColumns (table: Table) (promptText: string): Table * Column list =
    let rec chooseColumnsRec (tbl: Table) (acc: Column list) =
        if yesNo "Do you want to select a column?" then
            let col = chooseColumn tbl promptText
            // prompt for alias for the selected column
            let aliasInput = promptUser (sprintf "Enter alias for selected column '%s' (leave blank to keep current): " col.Name)
            let updatedCol =
                if String.IsNullOrWhiteSpace(aliasInput) then
                    col
                else
                    { col with Alias = Some aliasInput }
            // replace the column in the table's Columns list
            let updatedCols =
                tbl.Columns
                |> List.map (fun c -> if c.Name = col.Name then updatedCol else c)
            let updatedTable = { tbl with Columns = updatedCols }
            chooseColumnsRec updatedTable (updatedCol :: acc)
        else
            (tbl, List.rev acc)
    
    // prompt for alias for the table itself before choosing columns
    let alias = promptUser (sprintf "Enter alias for table '%s' (leave blank to skip): " table.Name)
    let newTable, columns = chooseColumnsRec table []

    // if alias is given, update the table's Alias property
    let finalTable =
        if String.IsNullOrWhiteSpace(alias) then
            newTable
        else
            { newTable with Alias = Some alias }
    (finalTable, columns)

// MAIN FUNCTION TO GET TABLE
let getTable () =
    let tableName = getTableName ()

    // get columns of table and their optional aliases
    let columns = getColumns tableName []

    // build table object
    let table = {
        Name = tableName
        Alias = None // alias is given in chooseColumns
        Columns = columns 
        |> List.map (
            fun (col) -> { 
                Name = col; 
                Alias = None // aliases are assigned in chooseColumns, so start with None here
            }
        )
    }

    // return table object
    table

// creating multiple tables
let rec addTables (acc: Table list) () : Table list =
    let okay = yesNo "Do you want to add a table?"
    if okay then
        let table = getTable ()
        addTables (table :: acc) ()
    else
        List.rev acc