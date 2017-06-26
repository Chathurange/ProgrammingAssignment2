## Caching the inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefits to caching the inverse of a matrix rather than compute it repeatedly.

## below function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Below function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
}