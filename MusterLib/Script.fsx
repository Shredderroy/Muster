// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "ListExtensions.fs"
open MusterLib

let mx = 1000000
let lst1 = [for i in 1 .. mx -> i]
let lst2 = [for i in 1 .. (mx + 1) -> i]

printfn "Starting"

lst2
|> ListExtensions.isExtensionOf lst1
|> printfn "%A"

printfn "Done"
