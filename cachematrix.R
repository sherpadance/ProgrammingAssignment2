## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This script contains a pair of functions that cache the inverse of a matrix.

## MakeCacheMatrix() creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    # Store inverse; initialize to NULL (no inverse stored at creation)
    inv <- NULL
    
    # Set matrix (store x and re-initialize mean)
    set <- function (newValue) {
        x <<- newValue
        inv <<- NULL
    } 
    # Get matrix (return x)
    get <- function() x
    
    # Set inverse value // assume matrix supplied is invertable
    setInverse <- function(inverse) inv <<- inverse
    
    # Get inverse value
    getInverse <- function() inv
    
    # list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Check for stored inverse value
    inv <- x$getInverse()
    
    # If inverse value found - return it
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    # Inverse value was not previously set, calculated it now and store for
    # future use
    
    # Get underlying data
    matrixdata <- x$get()
    # Calculate inverse value
    inv <- solve(matrixdata, ...)
    # Store for future use
    x$setInverse(inv)
    # Return calculated inverse
    inv
}