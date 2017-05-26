## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache a matrix
## and a object computed

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL 
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inv) invm <<- inv
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}
