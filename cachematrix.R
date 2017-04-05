## A pair of functions that create a special matrix object for caching
## The inverse matrix.

## Creates a "matrix" object that has methods for setting and getting data and
## Setting and getting the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)

}


## Checks object to see whether inverse data exists in an object made by 
## makeCacheMatrix(), if not it calculates that and sets it to avoid 
## Calculating again in future.Returns an object that is the inverse of 
## "matrix" x. 

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    message("solving for first time")
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
