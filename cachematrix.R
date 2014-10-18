## This project permits the caching of matrix inversion results
## to eliminate the time consuming problem when a matrix requires
## repeated inversion operations in long running loops. Two functions
## provide the abilty to achieve this objective:
## 
## MakeCacheMatrix() returns a special list containing 4 functions that
##   cache and retrieve a square invertible matrix and its inverse matrix
##
## cacheSolve() utilizes the list functions of the special list returned
##   by MakeCacheMatrix() to cache a matrix, calculate an inverse as
##   needed (whenever the cached matrix changes) and return its inverse


## MakeCacheMatrix() returns a special list containing 4 functions for
##   caching a matrix and its inverse
##     set() caches a matrix
##     get() returns the cached matrix
##     setinverse() caches the inverse of a matrix
##     getinverse() returns the cached matrix inverse
##     

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(matinv) inv <<- matinv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve() utilizes the list functions of the special list returned
##   by MakeCacheMatrix() to return the inverse of a cached matrix. The
##   function argument must be the special list returned by the 
##   MakeCacheMatrix() function. If an inverse has been previously 
##   cached, then it is simply returned from the cache. If not, the cached 
##   matrix is retrieved and its inverse created using the solve() function.
##   The resulting matrix inverse is cached and is the returned value

cacheSolve <- function(x, ...) {

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
