## Compute the inverse of a quadratic matrix and cache the result.
## Future calls to the cacheSolve function will not recompute the inverse
## but look it up in the cache.

## makeCacheMatrix
## provides all neccessary operations to get/set a matrix and its inverse
## serves as a data object to cache the calculated inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
## calculates the inverse of a matrix once and caches it
## answers any further calls by a lookup from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached matrix inverse")
          return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
