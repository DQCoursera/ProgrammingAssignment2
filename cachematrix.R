## The first function caches the inverse matrix
## The second function first checks if the inverse matrix has already been
## solved & cached and returns that, or it solves the cached inverse itself

## The makeCacheMatrix function first initializes the cached inverse to NULL
## Then it sets the matrix and resets the inverse cache
## It gets the matrix then sets the cached inverse and gets the cached inverse
## finally returns the list of functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Checks if the inverse has already been cached
## returns inverse if already cached
## calculates inverse, caches it, and returns it if not already cached

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}