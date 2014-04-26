## CacheMatrix will create a inverse of a matrix and store that matrix for
## future use. cachSolve with load the inverted matrix instead of calculating the inverse.

## This is the first function to cache the matrix.
## It takes the example code and substitutes the solve function for the mean function.
## "solve" with no arguments returns the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## This function will determine in the inverse of an input matrix is in memmory.
## If it is in memmory the matrix will be retrived and a message to that effect will be displayed.
## Otherwise it will use the previous function to calculate and store the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
