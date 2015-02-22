## makeCacheMatrix method takes a matrix and return a wrapper object for the matrix
## cacheSolve method uses the method created by makeCacheMatrix, solves and return the inverse of a matrix using caching

## Takes a matrix object as an argument and returns a wrapper object with
## methods to cache and retrieve reverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse<- function(y) inverse <<-y
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Takes matrix A created by makeCacheMatrix and returns its inverse
## If the solution is already cached then it retrieves the result from cache else it
## solves, caches and finally returns the result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    if (is.null(inverse)) {
        inverse <- calculateInverse(x$get()) ##Calculate the inverse of the matrix
        x$setInverse(inverse)
    }

    return(inverse)
}
