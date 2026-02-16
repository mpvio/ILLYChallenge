module HelperFunctions

open System

let promptUser (text: string) =
    printf "%s" text
    Console.ReadLine()

// yes/ no parser
let rec yesNo (text: string) =
    let r = promptUser (text + " (y/n): ")
    match r.ToLower() with
    | "y" | "yes" -> true
    | "n" | "no" -> false
    | _ -> 
        printfn "Please enter 'y' or 'n'."
        yesNo text


let chooseIndex (max: int) (promptText: string): int =
    let rec loop () =
        let input = promptUser promptText
        match Int32.TryParse(input) with
        | true, v when v >= 1 && v <= max -> v - 1
        | _ ->
            printfn "Please enter a number between 1 and %d." max
            loop ()
    loop ()