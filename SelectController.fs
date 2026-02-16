module SelectController

open TableModel
open TableController
open HelperFunctions
open System.Globalization
open System

let rec private createSelectFromTables (tables: Table list) (query: string Set) : (string Set * Table list) =
    let table = chooseTable tables "Select table to query:"
    // chooseColumns lets you assign aliases to columns, so the updated table object needs to be returned and update the tables list
    let updatedTable, columns = chooseColumns table "Select column to include in SELECT:"
    // update tables list
    let updatedTables =
        tables
        |> List.map (fun t ->
            if t.Name = table.Name then
                updatedTable
            else
                t)
    
    // add selected columns to query set
    let newQuery =
        columns
        |> List.fold (fun acc col -> Set.add (col.ColumnDisplay()) acc) query
    
    // ask if user wants to select from another table
    let more = yesNo "Do you want to select columns from another table?"
    if more then
        createSelectFromTables updatedTables newQuery
    else
        (newQuery, updatedTables)

let createSelect (tables: Table list) : (string * Table list) =
    let all = yesNo "Select all columns?"
    if all then
        ("*", tables)
    else
        let (colSet, updatedTables) = createSelectFromTables tables Set.empty
        (colSet |> Set.toList |> String.concat ", ", updatedTables)

//let rec private createSelectFromTables (tables: Table list) (query: string Set): string Set =
//    let table = chooseTable tables "Select table to query:"
//    let updatedTable, columns = chooseColumns table "Select column to include in SELECT:"
//    // add selected columns to query set
//    let newQuery = 
//        columns 
//        |> List.fold (fun acc col -> Set.add (sprintf "%s" (col.ColumnDisplay())) acc) query
//    // ask if user wants to select from another table
//    let more = yesNo "Do you want to select columns from another table?"
//    if more then
//        createSelectFromTables tables newQuery
//    else
//        newQuery

//let createSelect (tables: Table list): string =
//    let all = yesNo "Select all columns?"
//    if all then
//        "*"
//    else
//        // convert columns to string for SELECT clause
//        createSelectFromTables tables Set.empty |> Set.toList |> String.concat ", "
    