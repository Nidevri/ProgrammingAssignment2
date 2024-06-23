##This file contains 2 functions, the first of which (makeCacheMatrix()) creates an object that stores a matrix and its inverse.
##The second function (cacheSolve()) takes an object returned by makeCacheMatrix() and retrieves the inverted matrix from its environment.


## makeCacheMatrix takes a matrix as argument and stores this matrix and its inverse in the cached memory.
## The inverse will only be cached after an initial run of cacheSolve if the matrix is new

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will retrieve the inverse from a makeCacheMatrix object if available, if not it calculates the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
