##This ia a matrix inversion program that enable us to compute the inverse of a 
## matrix and cache this inverse. If the content of the matrix has not change 
## the inverse is needed, rather than computation, it's inverse will be pulled 
##from the cache

## This 1st function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This 2nd function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cache solve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        message("calculating the inverse")
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv) 
}
