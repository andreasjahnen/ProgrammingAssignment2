## The calculation of the inverse of a matrix function can
## be quite time consuming. The following functions allow that 
## a cached version of the inverse matrix is used. 
## 

## The cache function provides cache access methods in order
## to allow for access to the needed R environments.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(inverse) m <<- inverse
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## The cacheSolve function makes use the makeCacheMatrix functions in order to 
## return the cached matrix or if run the first time, calculates and return the 
## invers matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
