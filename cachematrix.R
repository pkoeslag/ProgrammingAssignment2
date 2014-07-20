## Provide caching for the calculation of matrix inversion.

## Create a cache for a matrix inversion result.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<-inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Invert a matrix using a matrix cache, i.e. only do the calculation once.
cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
