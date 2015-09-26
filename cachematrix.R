## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. By using the <<- operator, which 
## can be used to assign a value to an object in an 
## environment that is different from the current environment, we
## can cache the matrix inversion to avoid repeate calculation. 

## makeCacheMatrix creates a special "matrix", which is a list
## containing functions to 1)set the value of the matrix; 2)get 
## the value of the martrix; 3)set the martrix inversion ; 4)get 
## the martrix inversion.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) i <<- inversion
        getinversion <- function() i
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinversion()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinversion(i)
        i
}
