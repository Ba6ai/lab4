open System

// Тип дерева
type Tree =
    | Null
    | Head of int * Tree * Tree

// Функция автоматической вставки
let rec insert newValue tree =
    match tree with
    | Null -> Head(newValue, Null, Null)
    | Head(value, left, right) ->
        if newValue < value then
            Head(value, insert newValue left, right)
        else
            Head(value, left, insert newValue right)

// Функция генерации случайного дерева
let generateRandom count =
    let rnd = Random()
    let rec loop currentCount accTree =
        if currentCount = 0 then
            accTree
        else
            let randomNumber = rnd.Next(1, 21)
            loop (currentCount - 1) (insert randomNumber accTree)
    loop count Null

// Функция свертки fold
// Она обходит дерево и накапливает результат в аккумуляторе 'acc'
let rec treeFold f acc tree =
    match tree with
    | Null -> acc // Если null, возвращаем текущий аккумулятор
    | Head(value, left, right) ->
        // Приминение функции к текущему значению и старому поддереву
        let acc1 = f acc value
        // Рекурсивно передаёт в левое поддерево
        let acc2 = treeFold f acc1 left
        // Передаёт в правое поддерево
        treeFold f acc2 right

[<EntryPoint>]
let main _ =
    printf "Введите количество элементов в дереве: "
    let count = int (Console.ReadLine())
    
    let myTree = generateRandom count
    printfn "\nСгенерированное дерево: %A" myTree

    printf "\nВведите число для поиска в дереве: "
    let target = int (Console.ReadLine())

    // Если аккумулятор уже true ИЛИ текущее число совпадает с целью -> true
    let isFound = 
        treeFold (fun acc x -> acc || (x = target)) false myTree

    if isFound then
        printfn "Элемент %d НАЙДЕН в дереве." target
    else
        printfn "Элемента %d НЕТ в дереве." target

    0
