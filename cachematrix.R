## Inverse of a Matrix cache
## Matrix inversion is usually a costly computation and there may be benefit 
## to caching the inverse of a matrix rather than repeatedly compute it. 
## Beneath is a pair of functions used to store a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
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


## Function to compute the inverse of the matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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




