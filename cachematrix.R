## These functions make it possible to cache a matrix-inverse
## and thus retrieve that inverse without calculating it again

## This function creates the object that is need for caching the
## inverse of the matrix. It creates a list of 4 functions: set,
## get, setinv, getinv. where set sets the matrix object and sets
## the inverse to NULL, get gets the matrix object, setinv sets the
## inverse of the matrix and getinv gets the inverse that is stored.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## set the inverse to null initially
        set <- function(y) {
                x <<- y         ## if a new matrix is given change x
                inv <<- NULL    ## and reset the inverse
        }
        get <- function() x    ##if get is called return x
        setinv <- function(inverse) inv <<- inverse ## if setinv is called asign inv a given value inverse
        getinv <- function() inv  ## if getinv is called, return the inverse
        list(set = set, get = get, ##return the list with the functions
             setinv = setinv,
             getinv = getinv)
}


## This function tries to get the cached inverse of a given cachematrix object
## and if it is not cached, it creates it and then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## get the value of the inverse of x
        if(!is.null(inv)) { ## if the value is not null then return the value
                return(inv)
        }
        mtrx <- x$get() ## if the inverse wasn't there, get the matrix
        inv <- solve(mtrx, ...) ## get the inverse
        x$setinv(inv) ## set the inverse
        inv ## return the inverse
}
