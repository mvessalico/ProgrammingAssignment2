## Functions for creating an object to cache a matrix and it's inverse
## in order to prevent recomputation of the inverse each time it's needed

## Note: These functions assume the inputted data matrix is invertible

## Creates an object wrapper to store the matrix data and it's inverse
## Params: x - the original matix data to be stored
## Returns: List comprising of getter and setter functions for getting
##          a matrix, setting matrix data, getting the inverse, and setting
##          the inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    setMatrix <- function(matrix) {
        x <<- matrix
        matrixInverse <<- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverse <- function(inverse) {
        matrixInverse <<- inverse
    }
    getInverse <- function() {
        matrixInverse
    }
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## Solves the inverse of a matrix. Checks if the inverse has already been
## calculated before computing
## Params: x - a "cache matrix" object, created uing makeCacheMatrix
## Returns: Matrix that is the inverse of the input matrix

cacheSolve <- function(x, ...) {
    matrixInverse <- x$getInverse()
    if (!is.null(matrixInverse)) {
        return(matrixInverse)
    } else {
        matrix <- x$getMatrix()
        matrixInverse <- solve(matrix)
        x$setInverse(matrixInverse)
        matrixInverse
    }
}
