## The functions in this file allow the user to store a matrix
## and cache its inverse.
##
## While the inverse is retrieved it is only re-calculated,
## if the matrix has changed.

## This function allows the creation of a matrix with its inversed cached
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function retrieves the inverse of the matrix.
## It is taken from the cache if possible.
cacheSolve <- function(x, ...) {
    s <- x$getInverse()
    if (is.null(s)) {
        message("Caching inverse")
        i <- solve(x$get())
        x$setInverse(i)
        return(i)
    }
    return(s)
}
