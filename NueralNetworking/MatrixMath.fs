module MatrixMath

open System
open MatrixError
open Matrix

type MatrixMath() =
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

    member this.Add(augend:Matrix, addend:Matrix) =
        let folder i j (state:Matrix) elementOne elementTwo = (state.[i,j] <- elementOne + elementTwo; state)

        doubleFold folder (new Matrix(augend.Rows, augend.Columns)) augend addend

    member this.Subtract(minuend:Matrix, subtrahend:Matrix) =
        let folder i j (state:Matrix) elementOne elementTwo = (state.[i,j] <- elementOne - elementTwo; state)

        doubleFold folder (new Matrix(minuend.Rows, minuend.Columns)) minuend subtrahend

    member this.MultiplyScalar multiplicand (multiplier:double) = raise (NotImplementedException "MultiplyScalar")
    member this.Multiply multiplicand (multiplier:Matrix) = raise (NotImplementedException "Multiply")
    member this.Divide dividend divisor = raise (NotImplementedException "Divide")
    member this.DotProduct a b = raise (NotImplementedException "DotProduct")

    member this.Transpose(matrix:Matrix) =
        let folder i j (state:Matrix) element = (state.[j,i] <- element; state)

        matrix.foldiMatrix folder (new Matrix(matrix.Rows, matrix.Columns)) matrix

    member this.Identity size =
        if size < 1 then
            raise (MatrixError "Size of identity matrix cannot be less than 1.")

        let identityMatrix = new Matrix(size, size)

        for i = 0 to (size-1) do
            identityMatrix.[i,i] <- 1.0

        identityMatrix