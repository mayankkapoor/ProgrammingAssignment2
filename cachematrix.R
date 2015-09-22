## Matrix inversion is usually a costly computation. These pair of
## functions cache the inverse of a matrix.
## Example usage:
## > x = rbind(c(1, -1/4), c(-1/4, 1))         // Create a matrix x
## > cx <- makeCacheMatrix(x)                  // Create our special matrix
## > cx$get()                                  // Return the matrix
## > cacheSolve(cx)                            // Return the inverse
## > cacheSolve(cx)                            // Call the 2nd time, so return
##                                             // the cached inverse

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
