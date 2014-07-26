## Matrix Manipulation
## These functions will compute the inverse of a matrix by 
## using a cache mechanism, thus performing a faster operation.

## ps: when using these functions, we should never use the same variable to get
## the result of MakeCacheMatrix, like x<-MakeCacheMatrix(x) which will generate an 
## error when calling cacheSolve. This error occurs because in this case the x$get() 
## would return a makeCacheMatrix structure instead of the matrix.


# Creates the structure which will be used
# to calculate the inverse of a matrix with cache mechanism.
# if we want the inverse of matrix z, first we should call MakeCacheMatrix(z)
# and then we should use the return on cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL
        set <- function(y) {
                x <<- y
                cached_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cached_inverse <<- inverse
        getInverse <- function() cached_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of a matrix, by using cache mechanism.
## if there's no cached value, computes the inverse and caches result.
##
## input x: a makeCacheMatrix structure.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
