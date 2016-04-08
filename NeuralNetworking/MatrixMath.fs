module MatrixMath

open System
open System.Threading.Tasks
open MatrixError
open Matrix

let doubleFold (folder: int -> int -> 'S -> 'T -> 'T -> 'S) (state: 'S) (arrayOne: Matrix) (arrayTwo: Matrix) =
    if arrayOne.Rows <> arrayTwo.Rows then
        raise (MatrixError "Matrices must have the same number of rows to be summed.")
    if arrayOne.Columns <> arrayTwo.Columns then
        raise (MatrixError "Matrices must have the same number of columns to be summed.")

    let rec doubleFold' rowIdx colIdx state =
        if rowIdx = arrayOne.Rows then
            state
        elif colIdx = arrayOne.Columns then
            doubleFold' (rowIdx+1) 0 state
        else
            doubleFold' rowIdx (colIdx+1) (folder rowIdx colIdx state arrayOne.[rowIdx,colIdx] arrayTwo.[rowIdx,colIdx])

    doubleFold' 0 0 state

let DeleteCol (matrix:Matrix) deleted = 
    let foldr i j (state:Matrix) e =
        if j = deleted then
            state
        elif j > deleted then
            (state.[i,j-1] <- e; state)
        else
            (state.[i,j] <- e; state)

    matrix.foldiMatrix foldr (new Matrix(matrix.Rows, matrix.Columns - 1)) matrix

let DeleteRow (matrix:Matrix) deleted = 
    let foldr i j (state:Matrix) e =
        if i = deleted then
            state
        elif i > deleted then
            (state.[i-1,j] <- e; state)
        else
            (state.[i,j] <- e; state)

    matrix.foldiMatrix foldr (new Matrix(matrix.Rows - 1, matrix.Columns)) matrix

let Add(augend:Matrix, addend:Matrix) =
    let folder i j (state:Matrix) elementOne elementTwo = (state.[i,j] <- elementOne + elementTwo; state)

    doubleFold folder (new Matrix(augend.Rows, augend.Columns)) augend addend

let Subtract(minuend:Matrix, subtrahend:Matrix) =
    let folder i j (state:Matrix) elementOne elementTwo = (state.[i,j] <- elementOne - elementTwo; state)

    doubleFold folder (new Matrix(minuend.Rows, minuend.Columns)) minuend subtrahend

let MultiplyByScalar (multiplicand:Matrix) (multiplier:double) = 
    multiplicand.foldiMatrix (fun i j (state:Matrix) e -> (state.[i,j] <- e * multiplier; state)) (new Matrix(multiplicand.Rows, multiplicand.Columns)) multiplicand

let Multiply (multiplicand:Matrix) (multiplier:Matrix) =
    if multiplicand.Rows <> multiplier.Columns then
        raise (MatrixError "The multiplicand must have a number of rows equal to the number of columns in the multiplier.")

    let rowsA, colsA = multiplicand.Rows, multiplicand.Columns
    let rowsB, colsB = multiplier.Rows, multiplier.Columns
    let result = new Matrix(multiplicand.Rows, multiplier.Columns)

    Parallel.For(0, rowsA, (fun i ->
        for j = 0 to colsB - 1 do
            for k = 0 to colsA - 1 do
                result.[i,j] <- result.[i,j] + multiplicand.[i,k] * multiplier.[k,j]))  
    |> ignore

    result

let DivideByScalar (dividend:Matrix) divisor =
    dividend.foldiMatrix (fun i j (state:Matrix) e -> (state.[i,j] <- e / divisor; state)) (new Matrix(dividend.Rows, dividend.Columns)) dividend

let DotProduct a b = 
    doubleFold (fun i j state elementOne elementTwo -> state + (elementOne * elementTwo)) 0.0 a b

let Transpose(matrix:Matrix) =
    let folder i j (state:Matrix) element = (state.[j,i] <- element; state)

    matrix.foldiMatrix folder (new Matrix(matrix.Rows, matrix.Columns)) matrix

let Identity size =
    if size < 1 then
        raise (MatrixError "Size of identity matrix cannot be less than 1.")

    let identityMatrix = new Matrix(size, size)

    for i = 0 to (size-1) do
        identityMatrix.[i,i] <- 1.0

    identityMatrix