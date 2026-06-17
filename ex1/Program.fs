open System

type Tree =
    | Null
    | Head of int * Tree * Tree

let rec insert newValue tree =
    match tree with
    | Null -> Head(newValue, Null, Null)
    | Head(value, left, right) ->
        if newValue < value then
            Head(value, insert newValue left, right)
        else
            Head(value, left, insert newValue right)

let generateRandom count =
    let rnd = Random()

    let rec loop currentCount accTree = 
        if currentCount = 0 then
            accTree
        else
            let randomNumber = rnd.Next(1, 101)
            loop(currentCount - 1)(insert randomNumber accTree)
    
    loop count Null

let rec treeMap f tree =
    match tree with
    | Null -> Null
    | Head(value, left, right) -> 
        Head(f value, treeMap f left, treeMap f right)

let duplicateNumber (n: int) = 
    let s = string n
    int (s + s)

[<EntryPoint>]
let main _ =
    printf "Введите количество элементов в дереве: "
    let count = int (Console.ReadLine())
    
    let myTree = generateRandom count
    printfn "\nИсходное дерево: %A" myTree

    let newTree = treeMap duplicateNumber myTree
    printfn "\nНовое дерево: %A" newTree
    
    0
