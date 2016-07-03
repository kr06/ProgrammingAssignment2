## The overall function caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        set <- function(y){
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixInverse <<- inverse
        getInverse <- function() matrixInverse
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data)
        x$setInverse(matrixInverse)
        matrixInverse
}
