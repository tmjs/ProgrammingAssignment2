## The following two functions create an object that stores a matrix and
## caches it's inverse and then provides a function for calculating the inverse
## matrix of a given matrix using the object

## This function creates an object that stores a matrix and
## caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}

## This function calculates the inverse of a matrix created it the above
## function.  It first checks to see if the inverse has already been
## calculated.  If so, it gets the inverse from the cache and skips
## the computation. Otherwise it calculates the inverse of the matrix
## and sets the matrix via the setmatrix function.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting chached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
