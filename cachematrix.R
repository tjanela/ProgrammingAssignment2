## This file contains an extension to the default matrix behavior that allows
## the computation of the inverse matrix of an invertible matrix to be cached.

## The 'makeCacheMatrix' creates a list with the needed infrastructure
## extensions to the matrix object.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The 'cacheSolve' function contains the logic needed to compute the inverse of
## a matrix, cache it, and then serve that cached value on subsequent 
## invocations.
## 'x' is a cacheMatrix list
## This function returns the inverse matrix of an invertible matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
