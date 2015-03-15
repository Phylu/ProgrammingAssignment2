## The functions in this file allow the user to store a matrix
## and cache its inverse.
##
## While the inverse is retrieved it is only re-calculated,
## if the matrix has changed.

## This function allows the creation of a matrix with its inversed cached
makeCacheMatrix <- function(x = matrix()) {
    # Inverse of the matrix is stored in i
    # i is set to NULL when the matrix is created
    i = NULL
    set <- function(y) {
        # Setter updates matrix
        x <<- y
        # i is set to NULL when te matrix changes
        i <<- NULL
    }
    # Returns the original matrix
    get <- function() x
    # Store the inverse in i
    setInverse <- function(inverse) i <<- inverse
    # Returns the inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function retrieves the inverse of the matrix.
## It is taken from the cache if possible.
cacheSolve <- function(x, ...) {
    # Get the stored inverse 
    s <- x$getInverse()
    # If there is no inverse stored, solve it and cache it
    if (is.null(s)) {
        message("Caching inverse")
        s <- solve(x$get())
        x$setInverse(s)
    }
    # Return the inverse
    return(s)
}
