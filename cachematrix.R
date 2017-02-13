## Matrix inversion can be a costly computation so instead of computing
## over and over these functions make a object which stores a matrix and
## caches the inverse in order to save time

## This function creates a "matrix" object which caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
  mm <- NULL
  set <- function(y) {
    x <<- y
    mm <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) mm <<- inverse
  getInverse <- function() mm
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function get the inverse of the "matrix" made by makeCacheMatrix and
## gets the cache instead of re computing if the object hasn't changed

cacheSolve <- function(x, ...) {
  mm <- x$getInverse()
  if (!is.null(mm)) {
    message("getting cached data")
    return(mm)
  }
  
  mat <- x$get()
  mm <- solve(mat, ...)
  x$setInverse(mm)
  mm
}