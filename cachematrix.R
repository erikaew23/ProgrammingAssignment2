## Instead of computing it every time, a computation (in this case the inverse of a matrix) can be cached. The following functions create a special object that stores a matrix and caches its inverse.

## This function (makeCacheMatrix) caches the computation of the inverse of a matrix and stores it for easier access in the future.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## With the makeCacheMatrix function, this function (cacheSolve) computes the inverse of the matrix from the cache assuming the matrix is the same one used.

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
