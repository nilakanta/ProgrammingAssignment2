## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseValue <- NULL
    set <- function(y) {
        x <<- y
        inverseValue <<- NULL
    }
    get <- function()  x
    setInverse <- function(inverse) inverseValue <<- inverse
    getInverse <- function() inverseValue
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
    inverseValue <- x$getInverse()
    if (!is.null(inverseValue)) {
        message("getting cached inverse value.")
        return(inverseValue)
    }
    inputMatrix <- x$get()
    inverseValue <- solve(inputMatrix, ...)
    x$setInverse(inverseValue)
    inverseValue
}
