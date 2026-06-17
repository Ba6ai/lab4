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
            let randomNumber = rnd.Next(1, 21)
            loop (currentCount - 1) (insert randomNumber accTree)
    loop count Null

let rec treeFold f acc tree =
    match tree with
    | Null -> acc
    | Head(value, left, right) ->
        let acc1 = f acc value
        let acc2 = treeFold f acc1 left
        treeFold f acc2 right

[<EntryPoint>]
let main _ =
    printf "Введите количество элементов в дереве: "
    let count = int (Console.ReadLine())
    
    let myTree = generateRandom count
    printfn "\nСгенерированное дерево: %A" myTree

    printf "\nВведите число для поиска в дереве: "
    let target = int (Console.ReadLine())

    let isFound = 
        treeFold (fun acc x -> acc || (x = target)) false myTree

    if isFound then
        printfn "Элемент %d НАЙДЕН в дереве." target
    else
        printfn "Элемента %d НЕТ в дереве." target

    0
