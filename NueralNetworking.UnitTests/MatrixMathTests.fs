//TODO: Clean up conversions between floats and doubles
namespace NueralNetworking.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting

module MatrixMathTests =
    open System
    open Matrix
    open MatrixError
    open MatrixMath

    //TODO: Need more tests for add, subtract, transpose, and Identity
    [<TestClass>]
    type testrun() =
        let sut = new MatrixMath()

        let testMatrixOne = new Matrix(3,3)
        let testMatrixTwo = new Matrix(3,3)

        [<TestInitialize>]
        member x.setup() =
            testMatrixOne.[0,0] <- 0.0
            testMatrixOne.[0,1] <- 1.0
            testMatrixOne.[0,2] <- 2.0
            testMatrixOne.[1,0] <- 3.0
            testMatrixOne.[1,1] <- 4.0
            testMatrixOne.[1,2] <- 5.0
            testMatrixOne.[2,0] <- 6.0
            testMatrixOne.[2,1] <- 7.0
            testMatrixOne.[2,2] <- 8.0
            
            testMatrixTwo.[0,0] <- 0.0
            testMatrixTwo.[0,1] <- 10.0
            testMatrixTwo.[0,2] <- 12.0
            testMatrixTwo.[1,0] <- 13.0
            testMatrixTwo.[1,1] <- 14.0
            testMatrixTwo.[1,2] <- 15.0
            testMatrixTwo.[2,0] <- 16.0
            testMatrixTwo.[2,1] <- 17.0
            testMatrixTwo.[2,2] <- 18.0                     

        [<TestMethod>]
        member x.add_GivenValidArguments_ReturnsMatrixThatIsTheSumOfTheInputs() =            
            let expectedResults = new Matrix(3,3)
            expectedResults.[0,0] <- 0.0
            expectedResults.[0,1] <- 11.0
            expectedResults.[0,2] <- 14.0
            expectedResults.[1,0] <- 16.0
            expectedResults.[1,1] <- 18.0
            expectedResults.[1,2] <- 20.0
            expectedResults.[2,0] <- 22.0
            expectedResults.[2,1] <- 24.0
            expectedResults.[2,2] <- 26.0

            let result = sut.Add(testMatrixOne, testMatrixTwo)

            Assert.AreEqual(expectedResults, result)

        [<TestMethod>]
        member x.subtract_GivenValidArguments_ReturnsMatrixThatIsTheDifferenceOfTheInputs() =            
            let expectedResults = new Matrix(3,3)
            expectedResults.[0,0] <- 0.0
            expectedResults.[0,1] <- 9.0
            expectedResults.[0,2] <- 10.0
            expectedResults.[1,0] <- 10.0
            expectedResults.[1,1] <- 10.0
            expectedResults.[1,2] <- 10.0
            expectedResults.[2,0] <- 10.0
            expectedResults.[2,1] <- 10.0
            expectedResults.[2,2] <- 10.0

            let result = sut.Subtract(testMatrixTwo, testMatrixOne)

            Assert.AreEqual(expectedResults, result)

        [<TestMethod>]
        member x.transpose_GivenValidArguments_ReturnsMatrixThatIsTheTranspositionOfTheInput() =            
            let expectedResults = new Matrix(3,3)
            expectedResults.[0,0] <- 0.0
            expectedResults.[0,1] <- 3.0
            expectedResults.[0,2] <- 6.0
            expectedResults.[1,0] <- 1.0
            expectedResults.[1,1] <- 4.0
            expectedResults.[1,2] <- 7.0
            expectedResults.[2,0] <- 2.0
            expectedResults.[2,1] <- 5.0
            expectedResults.[2,2] <- 8.0

            let result = sut.Transpose(testMatrixOne)

            Assert.AreEqual(expectedResults, result)

        [<TestMethod>]
        member x.identity_GivenValidArguments_ReturnsIdentityMatrixOfTheGivenSize() =            
            let expectedResults = new Matrix(3,3)
            expectedResults.[0,0] <- 1.0
            expectedResults.[0,1] <- 0.0
            expectedResults.[0,2] <- 0.0
            expectedResults.[1,0] <- 0.0
            expectedResults.[1,1] <- 1.0
            expectedResults.[1,2] <- 0.0
            expectedResults.[2,0] <- 0.0
            expectedResults.[2,1] <- 0.0
            expectedResults.[2,2] <- 1.0

            let result = sut.Identity(3)

            Assert.AreEqual(expectedResults, result)