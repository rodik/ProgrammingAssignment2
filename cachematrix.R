## File contains functions for managing matrices with
## cached copies of their inverses

## Creates a matrix structure which contains its cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # inverse placeholder
    
    # internal functions for managing the structure
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates (or returns a cached copy of) the inverse of a matrix created by 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse() # read the existing inverse value
    if(!is.null(inv)) { # return the cached copy if it exists
        message("getting cached data")
        return(inv)
    }
    data <- x$get() # get the original matrix
    inv <- solve(data, ...) # calculate the inverse
    x$setinverse(inv)
    inv
}
