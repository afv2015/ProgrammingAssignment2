##
## There are two functions provided in this file.  
##
## The first, makeCacheMatrix, is a function supporting the creation of a 
## special "matrix".  It stores a numeric matrix and caches its inverse, once 
## it has been computed.  This is useful in reducing CPU load and compute 
## times, when the inverse is required in multiple computations.  This is a 
## technique generally called memoization.

## This second function, cacheSolve, computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then cachesolve retrieves
## the inverse from the cache.


## This function, creates a special "matrix". It supports functions to get 
## and set the value of the "matrix" and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv<- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of any special "matrix" 
## that is created using makeCacheMatrix. 
##
## It first checks whether the inverse has already been calculated. 
## If so, it retrieves the cached value and returns it.
## If not, it computes the inverse of the matrix, caches it, and returns it.
## The caching is executedusing the setinv() function defined in makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # The cache is read and checked.  
    # If non-NULL, return the cached value and exit
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # The original matrix is retrieved and the inverse computed.
    
    data <- x$get()
    inv <- solve(x)
    
    # The inverse is cached and its value returned.
    
    x$setinv(inv)
    inv
}
