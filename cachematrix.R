## This experiment involves writing two functions that work together to cache the 
## inverse of a matrix.

## This first function creates and stores a special "matrix" and caches the inverse of that 
## special "matrix".

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

## The second function calculates the inverse of the matrix, but first it checks if that inverse
## has been calculated previously (and the matrix is unchanged). If so, it retrieves the inverse 
## from the cache and skips the calculation step. If not, it calculates the inverse of the matrix 
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
