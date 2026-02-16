module JoinController

open System
open HelperFunctions
open TableModel
open TableController
open JoinModel

// get type of join
let rec private getJoinType () =
    let r = promptUser "Enter join type (inner or i/ left or l/ right or r/ full or f): "
    match r.ToLower() with
    | "inner" | "i" -> "inner"
    | "left" | "l" -> "left"
    | "right" | "r" -> "right"
    | "full" | "f" -> "full"
    | _ ->
        printfn "Invalid join type. Please enter 'inner', 'left', 'right', or 'full'."
        getJoinType ()

// map user input to join type
let private mapStringToJoinType (s: string) =
    match s.ToLowerInvariant() with
    | "inner" -> JoinType.Inner
    | "left" -> JoinType.Left
    | "right" -> JoinType.Right
    | "full" -> JoinType.Full
    | other ->
        printfn "unrecognised input so defaulting to INNER."
        JoinType.Inner

// select two tables, then join type, then columns to join on
let private createJoinFromTables (tables: Table list): Join =
    printfn "Select left table to join:"
    let left = chooseTable tables "Enter number of left table: "
    printfn "Select right table to join:"
    let right = chooseTable tables "Enter number of right table: "

    let joinTypeString = getJoinType ()
    let joinType = mapStringToJoinType joinTypeString

    printfn "Choose column from left table '%s' to join on:" (left.TableRef())
    let leftCol = chooseColumn left "Enter number of left column: "

    printfn "Choose column from right table '%s' to join on:" (right.TableRef())
    let rightCol = chooseColumn right "Enter number of right column: "

    // create and return join
    { 
      JoinType = joinType
      LeftTable = left
      RightTable = right
      LeftColumn = leftCol
      RightColumn = rightCol 
    }

let rec private joinMaker (acc: Join list) (tables: Table list) (): Join list =
    if yesNo "Do you want to add a join?" then
        let j = createJoinFromTables tables
        joinMaker (j :: acc) (tables: Table list) ()
    else
        List.rev acc

/// MAIN FUNCTION TO GET JOINS
let getJoins (tables: Table list) () : Join list =
    joinMaker [] tables ()


