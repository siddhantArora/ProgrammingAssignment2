

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    my_inv <- NULL
    set <- function(y) {
        x <<- y
        my_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) my_inv <<- inverse
    getinverse <- function() my_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    my_inv <- x$getinverse()
    if(!is.null(my_inv)) {
        message("getting cached data.")
        return(my_inv)
    }
    data <- x$get()
    my_inv <- solve(data)
    x$setinverse(my_inv)
    my_inv
}