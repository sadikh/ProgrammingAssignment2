## 'makeCacheMatrix' and 'cacheSolve' are a pair of functions that allow
## caching the inverse of a matrix to avoid multiple calculations.
##
## Sample usage:
## m <- matrix(1:4, 2, 2)
## cachedmatrix <- makeCacheMatrix(m)
## inv <- cacheSolve(cachedmatrix)
##
## 'makeCacheMatrix' returns a list that combines a matrix and its inverse.
## The matrix can be accessed via '$get()' and changed via '$set()'.
## The inverse can be accessed via '$getinverse()' and changed via '$setinverse()'.

makeCacheMatrix <- function(x = matrix()) {
    # the cached inverse
    inv <- NULL
    
    # getter and setter for the matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    # getter and setter for the inverse
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    #return list of all functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 'cacheSolve' accepts a matrix bundle created by 'makeCacheMatrix',
## and returns the inverse of the matrix. It uses the inverse matrix cached
## in the bundle if already calculated, otherwise it calculates and stores the
## inverse. Any additional parameters are passed on to 'solve'.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # inverse matrix if the inverse is not cached
    if(is.null(inv)){
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
    }
    
    # return the inverse
    inv
}
