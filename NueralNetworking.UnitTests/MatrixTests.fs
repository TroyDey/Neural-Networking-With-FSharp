//TODO: Clean up conversions between floats and doubles
namespace NueralNetworking.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting

module MatrixTests =
    open System
    open Matrix
    open MatrixError

    [<TestClass>]
    type testrun() =
        let testArray = Array2D.zeroCreate 3 3

        [<TestInitialize>]
        member x.setup() =
            testArray.[0,0] <- 0.0
            testArray.[0,1] <- 1.0
            testArray.[0,2] <- 2.0
            testArray.[1,0] <- 3.0
            testArray.[1,1] <- 4.0
            testArray.[1,2] <- 5.0
            testArray.[2,0] <- 6.0
            testArray.[2,1] <- 7.0
            testArray.[2,2] <- 8.0            

        [<TestMethod>]
        member x.constructMatrix_GivenValidArguments_DoesNotReturnNull() =
            let result = new Matrix(5, 5)                     
            Assert.IsNotNull result
            Assert.IsInstanceOfType(result, typedefof<Matrix>)

        [<TestMethod>]
        member x.add_GivenValidArguments_AddsTheGivenValueToTheSpecifiedElementOfTheMatrix() =
            let sut = new Matrix(5,5)

            let result = sut.Add(0,0,10.0)

            Assert.AreEqual(10.0, result)

        [<TestMethod>]
        [<ExpectedException(typedefof<MatrixError>)>]
        member x.add_GivenRowIndexLessThanZero_ThrowsMatrixErrorException() =
            let sut = new Matrix(5,5)

            sut.Add(-1,0,10.0) |> ignore

        [<TestMethod>]
        [<ExpectedException(typedefof<MatrixError>)>]
        member x.add_GivenColumnIndexLessThansZero_ThrowsMatrixErrorException() =
            let sut = new Matrix(5,5)

            sut.Add(0,-1,10.0) |> ignore

        [<TestMethod>]
        [<ExpectedException(typedefof<MatrixError>)>]
        member x.add_GivenRowIndexGreaterThanNumberOfRows_ThrowsMatrixErrorException() =
            let sut = new Matrix(5,5)

            sut.Add(5,0,10.0) |> ignore

        [<TestMethod>]
        [<ExpectedException(typedefof<MatrixError>)>]
        member x.add_GivenColumnIndexGreaterThanNumberOfColumns_ThrowsMatrixErrorException() =
            let sut = new Matrix(5,5)

            sut.Add(0,5,10.0) |> ignore

        [<TestMethod>]        
        member x.clear_GivenMatrixThatHasNonDefaultValues_UpdatesAllValuesToDefault() =
            let sut = new Matrix(5,5)
            sut.[0,0] <- 10.0

            sut.Clear()

            Assert.AreEqual(0.0, sut.[0,0])

        [<TestMethod>]
        member x.equals_GivenEqualMatricies_ReturnsTrue() =
            let sut = new Matrix(testArray)
            let comparisionMatrix = new Matrix(testArray)

            let result = sut.Equals(comparisionMatrix)

            Assert.IsTrue(result)

        [<TestMethod>]
        member x.equals_GivenNonEqualMatricies_ReturnsFalse() =
            let sut = new Matrix(testArray)
            let comparisionMatrix = new Matrix(3,3)
            comparisionMatrix.[0,0] <- 20.0
            comparisionMatrix.[0,1] <- 21.0
            comparisionMatrix.[0,2] <- 22.0

            let result = sut.Equals(comparisionMatrix)

            Assert.IsFalse(result)

        //TODO: Equals tests around precision
        
        [<TestMethod>]
        member x.fromPackedArray_GivenValidArray_UpacksTheArray() = 
            let sut = new Matrix(3,3)           
            let expectedResult = new Matrix(testArray)
            let packedArray = [|0.0;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|]

            sut.FromPackedArray(packedArray)   
                   
            Assert.IsTrue(sut.Equals(expectedResult))

        [<TestMethod>]
        member x.fromPackedArray_GivenArrayOfSizeZero_EmptyMatrix() = 
            let sut = new Matrix(0,0)            
            let expectedResult = new Matrix(0,0)
            let packedArray = [||]

            sut.FromPackedArray(packedArray)     
                   
            Assert.IsTrue(sut.Equals(expectedResult))

        [<TestMethod>]
        [<ExpectedException(typedefof<MatrixError>)>]
        member x.fromPackedArray_GivenArrayOfTheWrongSize_ThrowsMatrixErrorException() = 
            let sut = new Matrix(5,5)
            let packedArray = [|0.0;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|]

            sut.FromPackedArray(packedArray)

        [<TestMethod>]
        [<ExpectedException(typedefof<ArgumentNullException>)>]
        member x.fromPackedArray_GivenNull_ThrowsArgumentNullException() = 
            let sut = new Matrix(3,3)    

            sut.FromPackedArray(null)

        [<TestMethod>]
        member x.getCol_GivenValidColumnIndex_ReturnsNewColumnMatrix() =            
            let sut = new Matrix(testArray)        
            let expectedResult = new Matrix(3,1)
            
            expectedResult.[0,0] <- 0.0
            expectedResult.[1,0] <- 3.0
            expectedResult.[2,0] <- 6.0

            let result = sut.GetCol 0 
                   
            Assert.IsTrue(result.Equals(expectedResult))

        [<TestMethod>]
        [<ExpectedException(typedefof<ArgumentOutOfRangeException>)>]
        member x.getCol_GivenColumnIndexLessThanZero_ThrowsArgumentOutOfRangeException() =            
            let sut = new Matrix(3,3)

            sut.GetCol -1 |> ignore

        [<TestMethod>]
        [<ExpectedException(typedefof<ArgumentOutOfRangeException>)>]
        member x.getCol_GivenColumnIndexGreaterThanNumberOfColumns_ThrowsArgumentOutOfRangeException() =            
            let sut = new Matrix(3,3)

            sut.GetCol 3 |> ignore

        [<TestMethod>]
        member x.getRow_GivenValidRowIndex_ReturnsNewRowMatrix() =            
            let sut = new Matrix(testArray)        
            let expectedResult = new Matrix(1,3)
            
            expectedResult.[0,0] <- 0.0
            expectedResult.[0,1] <- 1.0
            expectedResult.[0,2] <- 2.0

            let result = sut.GetRow 0 
                   
            Assert.IsTrue(result.Equals(expectedResult))

        [<TestMethod>]
        [<ExpectedException(typedefof<ArgumentOutOfRangeException>)>]
        member x.getRow_GivenRowIndexLessThanZero_ThrowsArgumentOutOfRangeException() =            
            let sut = new Matrix(3,3)

            sut.GetRow -1 |> ignore

        [<TestMethod>]
        [<ExpectedException(typedefof<ArgumentOutOfRangeException>)>]
        member x.getRow_GivenRowIndexGreaterThanNumberOfColumns_ThrowsArgumentOutOfRangeException() =            
            let sut = new Matrix(3,3)

            sut.GetRow 3 |> ignore

        [<TestMethod>]
        member x.isZero_GivenArrayThatIsAllZero_ReturnsTrue() =
            let sut = new Matrix(3,3)

            Assert.IsTrue(sut.IsZero())

        [<TestMethod>]
        member x.isZero_GivenArrayThatIsNotAllZeros_ReturnsFalse() =
            let sut = new Matrix(testArray)

            Assert.IsFalse(sut.IsZero())

        [<TestMethod>]
        member x.isVector_GivenMatrixWithOneColumn_ReturnsTrue() =
            let sut = new Matrix(3,1)

            Assert.IsTrue(sut.IsVector())

        [<TestMethod>]
        member x.isVector_GivenMatrixWithOneRow_ReturnsTrue() =
            let sut = new Matrix(1,3)

            Assert.IsTrue(sut.IsVector())

        [<TestMethod>]
        member x.isVector_GivenSingleElementMatrix_ReturnsTrue() =
            let sut = new Matrix(1,1)

            Assert.IsTrue(sut.IsVector())

        [<TestMethod>]
        member x.isVector_GivenMatrixWithMoreThanOneColumnAndRow_ReturnsFalse() =
            let sut = new Matrix(3,3)

            Assert.IsFalse(sut.IsVector())

//        [<TestMethod>]
//        member x.randomize_GivenValidMinAndMax_UpdatesTheMatrixWithRandomValuesInEachPosition() =
//            let sut = new Matrix(3,3)
//            //Need to mock the random function properly test this
            
        [<TestMethod>]
        member x.sum_GivenValidMatrix_ReturnsTheSumOfAllElementsInTheMatrix() =
            let sut = new Matrix(testArray)

            let result = sut.Sum()

            Assert.AreEqual(36.0, result)

        [<TestMethod>]
        member x.sum_GivenEmptyMatrix_ReturnsTheSumOfAllElementsInTheMatrix() =
            let sut = new Matrix(0,0)

            let result = sut.Sum()

            Assert.AreEqual(0.0, result)