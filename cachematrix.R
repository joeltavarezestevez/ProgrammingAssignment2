## The next functions makes a cache of the inverse of a matrix

# The Function makeCacheMatrix creates a list, but first, that is what it do:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of inverse of the matrix
# - get the value of inverse of the matrix
# Then it attach this values into a list

makeCacheMatrix <- function(x = matrix()) {
    MatrixInverse <- NULL
    set <- function(y) {
        x <<- y
        MatrixInverse <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) MatrixInverse <<- inverse
    getinv <- function() MatrixInverse
    list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}

# The next function returns the inverse of a matrix, assumming the matrix is always invertible.
#It takes the inverse matrix cached, in case is not cached, it creates the cache inverse matrix.

cacheSolve <- function(x, ...) {
    MatrixInverse <- x$getinverse()
    if(!is.null(MatrixInverse)) {
        message("getting cached matrix...")
        return(MatrixInverse)
    }
    data <- x$get()
    MatrixInverse <- solve(data)
    x$setinverse(MatrixInverse)
    MatrixInverse
}
