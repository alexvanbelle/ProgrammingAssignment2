## These functions enables to cache the solved matrix of a
## matrix to avoid computing it many times

## This function creates an object that stores a matrix and its solved one

makeCacheMatrix <- function(x = matrix()) {
    solvedMatrix <- NULL
    set <- function(y) {
        x <<- y
        solvedMatrix <- NULL
    }
    get <- function() x
    setSolve <- function(invert) solvedMatrix <<- invert
    getSolve <- function() solvedMatrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function enables to get the solved matrix at the lower computation cost
## it uses the cache one if available or computes it and store it

cacheSolve <- function(x, ...) {
    solved <- x$getSolve()
    if (!is.null(solved)){
        # solved one is already computed and cached
        return(solved)
    }
    data <- x$get()
    solved <- solve(data, ...)
    x$setSolve(solved)
    solved
}
