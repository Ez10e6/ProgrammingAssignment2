## These functions make it possible to cache a matrix-inverse
## and thus retrieve that inverse without calculating it again

## This function creates the object that is need for caching the
## inverse of the matrix. It creates a list of 4 functions: set,
## get, setinv, getinv. where set sets the matrix object and sets
## the inverse to NULL, get gets the matrix object, setinv sets the
## inverse of the matrix and getinv gets the inverse that is stored.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function tries to get the cached inverse of a given cachematrix object
## and if it is not cached, it creates it and then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinv(inv)
        inv
}
