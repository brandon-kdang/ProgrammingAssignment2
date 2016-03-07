## Methods to provide Caching the Inverse of a Matrix

## Make a cacheMatrix which is special list containing 
## the getters and setters for the matrix and its solve value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(val) m <<- val
    getSolve <- function() m
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Get solve value of a cacheMatrix
## If a cache value is there, it will be used.
## Otherwise a solve value is derived and stored in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    x$setSolve(solve(x$get(), ...))
    x$getSolve()
}
