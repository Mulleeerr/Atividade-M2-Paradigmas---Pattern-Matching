    
//Questao 1 -------------------------------------------------------    

    let rec produtoImpares lista =
        match lista with
        | [] -> 1
        | head::tail ->
            match head with
            | x when x % 2 <> 0 -> x * produtoImpares tail
            | _ -> produtoImpares tail

    let listadeValores = [1; 2; 3; 4; 5; 6; 7]

    let produto = produtoImpares listadeValores

    printfn "Questao 1: Produto Numeros Impares %i" produto

//Questao 2-----------------------------------------------------------

    let rec operacaoValores x y =
        match x, y with
        | _, 0 -> 1
        | _, 1 -> x
        | 0, _ -> 0
        | 1, _ -> 1
        | _, _ when x = y -> x * y
        | _, _ ->
            let rec potencia valor vezes resultado = // caso forem diferentes, entra na funcao recursiva
                match vezes with
                | 0 -> resultado
                | _ -> potencia valor (vezes - 1) (resultado * valor)
            potencia x y 1

    let multiplica = operacaoValores 2 8
    printfn "Questao 2 A: O Resultado da Multiplicacao ou Valor elevado é %i" multiplica
    let multiplica2 = operacaoValores 3 3
    printfn "Questao 2 B: O Resultado da Multiplicacao ou Valor elevado é %i" multiplica2


//Questao 3 ------------------------------------------------------- 

    let valorPrimo num =
        match num with
        | num when num <= 1 -> false
        | _ ->
            let rec verificarDivisores divisor =
                match divisor * divisor > num with // verifica se o quadrado do divisor e maior que o numero
                | true -> true
                | _ ->
                    match num % divisor = 0 with // se for divisivel pelo divisor, nao e primo
                    | true -> false
                    | _ -> verificarDivisores (divisor + 1)
            verificarDivisores 2

    let numero = 13
    let verificaPrimo = valorPrimo numero
    match verificaPrimo with
        | true -> printfn "%d é um número primo" numero
        | false -> printfn "%d não é um número primo" numero


//Questao 4 ------------------------------------------------------- 

    let rec somaNumerosPrimos lista =
        match lista with
        | [] -> 0
        | primeiro::ultimo ->
            if valorPrimo primeiro then
                primeiro + somaNumerosPrimos ultimo
            else
                somaNumerosPrimos ultimo

    let listaNumeros = [1;2;3;4;5;6;7;8;9;10;11;13;15;18]
    let resultadoSomaPrimos = somaNumerosPrimos listaNumeros
    printfn "O Resultado da soma dos numeros primos da lista é %i" resultadoSomaPrimos