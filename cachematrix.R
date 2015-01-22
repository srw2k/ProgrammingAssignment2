## Together these functions allow a user to create and work with
## a wrapper for a matrix together with its inverse, the latter 
## being cached for quick future access.

## This function is called when the user wants to create a
## wrapper for a matrix with its inverse to be cached,
## the inverse being set to NULL initially.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(new_inverse) {
        inverse <<- new_inverse
    }
    getinverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the wrapped matrix 
## created with the above function. It first checks to see 
## if the inverse has already been computed. If so, it gets 
## the inverse from the cache and skips the computation. 
## If not, it computes the inverse of the matrix and sets 
## the value of the inverse in the cache via the 
## setinverse() function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
