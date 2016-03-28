//TODO: Clean up conversions between floats and doubles
module Matrix

open System
open MatrixError

//TODO: Rename to double matrix as that is what it actually is (and all that is needed for the purposes of this app) or
//figure out how to get 'T to work with things like addition, could maybe use some kind of function or type that knows how
//to add 'T's and use that instead of addition and the like directly
type Matrix(inputMatrix:double[,]) =
    let mutable internalArray = inputMatrix
    let internalRows = Array2D.length1 inputMatrix
    let internalCols = Array2D.length2 inputMatrix
    //let mutable internal1DArray = inputMatrix |> Seq.cast<double> |> Seq.toArray could use this 1-d array version for certain operations    

    let validateArguments rowIdx colIdx =        
        match (rowIdx,colIdx) with
        | _ when rowIdx < 0 -> raise (MatrixError "Row index cannot be less than 0")
        | _ when rowIdx >= Array2D.length1 internalArray -> raise (MatrixError "Row index cannot be greater than or equal to the number of rows")
        | _ when colIdx >= Array2D.length2 internalArray -> raise (MatrixError "Column index cannot be greater than or equal to the number of columns")
        | _ when colIdx < 0 -> raise (MatrixError "Column index cannot be less than 0")
        | _ -> ()    

    let foldiWhile (folder: int -> int -> 'S -> 'T -> 'S) (terminator: int -> int -> 'S -> 'T -> bool) (state: 'S) (array: 'T[,]) =
        let rec foldiWhile' rowIdx colIdx state =
            if rowIdx = Array2D.length1 internalArray then
                state
            elif colIdx = Array2D.length2 internalArray then
                foldiWhile' (rowIdx+1) 0 state
            elif (terminator rowIdx colIdx state array.[rowIdx,colIdx]) then
                folder rowIdx colIdx state array.[rowIdx,colIdx]
            else
                foldiWhile' rowIdx (colIdx+1) (folder rowIdx colIdx state array.[rowIdx,colIdx])
        foldiWhile' 0 0 state

    let inPlaceMap (maper: 'T -> 'T) (array: 'T[,]) =
        let rec inPlaceMap' rowIdx colIdx =
            if rowIdx = Array2D.length1 internalArray then
                ()
            elif colIdx = Array2D.length2 internalArray then
                inPlaceMap' (rowIdx+1) 0
            else
                array.[rowIdx,colIdx] <- maper array.[rowIdx,colIdx]
                inPlaceMap' rowIdx (colIdx+1)
        inPlaceMap' 0 0

    new(numberOfRows: int, numberOfColumns: int) =
        Matrix(Array2D.zeroCreate<double> numberOfRows numberOfColumns)

    new(matrix:Matrix) =
        Matrix(Array2D.init matrix.Rows matrix.Columns (fun n m -> matrix.[n,m]))

    override this.Equals(matrix) =
        this.EqualsToPrecision(matrix :?> Matrix, 10.0)

    //TODO: This is not sufficient as a hash code should identify the object while minimizing collisions
    //hashing each element of the matrix and combining them will be cost prohibitive, maybe a subset?
    override this.GetHashCode() = 
        this.Rows.GetHashCode()

    member this.Item
        with get(n: int, m:int) = internalArray.[n,m]
        and set(n: int, m: int) (value: double) = internalArray.[n,m] <- value

    member this.Size
        with get() = this.Rows * this.Columns
    
    member this.Rows
        with get() = internalRows

    member this.Columns
        with get() = internalCols

    member this.ToPackedArray() = internalArray |> Seq.cast<double> |> Seq.toArray

    member this.To2DArray() = Array2D.copy internalArray

    member this.Clone() =
        Matrix(Array2D.copy internalArray)

    member this.foldiMatrix (folder: int -> int -> 'S -> double -> 'S) (state: 'S) (matrix: Matrix) =
        let rec foldi' rowIdx colIdx state =
            if rowIdx = Array2D.length1 internalArray then
                state
            elif colIdx = Array2D.length2 internalArray then
                foldi' (rowIdx+1) 0 state
            else
                foldi' rowIdx (colIdx+1) (folder rowIdx colIdx state matrix.[rowIdx,colIdx])
        foldi' 0 0 state

    member this.Add(n,m,adden) =
        validateArguments n m                       
        internalArray.[n,m] <- internalArray.[n,m] + adden
        internalArray.[n,m]

    member this.Clear() =
        inPlaceMap (fun _ -> 0.0) internalArray
    
    member this.EqualsToPrecision(matrix:Matrix, precision:double) =
        if precision < 0.0 then
            raise (ArgumentOutOfRangeException "precision")
        
        let test = Math.Pow(10.0, precision)
        let longPrecision = int64 test

        if Double.IsInfinity(test) || longPrecision > Int64.MaxValue then
            raise (MatrixError ("Precision of " + precision.ToString() + " decimal places is not supported."))
                    
        let checkEquality n m state element = ((int64 element) * longPrecision) <> int64 ((int64 matrix.[n,m]) * longPrecision)

        (not (foldiWhile checkEquality checkEquality false internalArray))

    member this.FromPackedArray(packedArray:double[]) =
        if packedArray = null then
            raise (ArgumentNullException "packedArray")
        if packedArray.Length <> this.Size then
            raise (MatrixError "The input array must have a length of rows * columns in order to be unpacked into this matrix.")

        let rec fromPackedArray' rowIdx colIdx packedIdx =
            if rowIdx = this.Rows then
                ()
            elif colIdx = this.Columns then
                fromPackedArray' (rowIdx+1) 0 packedIdx
            else
                internalArray.[rowIdx,colIdx] <- packedArray.[packedIdx]
                fromPackedArray' rowIdx (colIdx+1) (packedIdx+1)
                
        fromPackedArray' 0 0 0     

    member this.GetCol col =
        if col < 0 || col >= this.Columns then
            raise (ArgumentOutOfRangeException "col")

        let colMatrix = Array2D.init this.Rows 1 (fun n m -> internalArray.[n,col])
        new Matrix(colMatrix)

    member this.GetRow row =
        if row < 0 || row >= this.Rows then
            raise (ArgumentOutOfRangeException "row")
        let rowMatrix = Array2D.init 1 this.Columns (fun n m -> internalArray.[row,m])
        new Matrix(rowMatrix)

    member this.IsVector () =
        this.Rows = 1 || this.Columns = 1

    member this.IsZero () =
        let checkZero n m state element = element <> 0.0
        (not (foldiWhile checkZero checkZero false internalArray))

    member this.Randomize min max = 
        let rand = new Random()
        inPlaceMap (fun _ -> rand.NextDouble() * (max - min) + min) internalArray

    member this.Sum () =
        let terminator n m state element = false
        let foldr n m state element = state + element
        foldiWhile foldr terminator 0.0 internalArray    