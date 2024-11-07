## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse matrix.
## For this functions we assume, that the matrix supplied is always invertible,
## that means a inverse matrix always exists.


## The following function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse matrix
## (4) get the value of the inverse matrix
## The <<- operator in this functionn is used to assign a value to an
## object in an environment that is different from the current environment

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


## The following function calculates the inverse matrix of the special "vector"
## created with the above function.
## However, it first checks to see if the inverse matrix has already been
## calculated. If so, it gets the inverse matrix from the cache and skips the
## computation. Otherwise, it calculates the inverse matrix of the data and sets
## the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
