## This file contains tow fucntions.They can be used to create a special "matrix",
##   which can cache its own inverse matrix once the inverse is calculated. Thus
##   if the inverse is used again, we can save a lot of calculations.

## Function "makeCacheMatrix" creates a list containg the following functions:
##   set(x): to set the value of matrix as x.
##   get(): to get the value of matrix.
##   setinv(inv): to set the cached inverse matrix as inv.
##   getinv(): to get the cached inverse matrix. Note if there are NO inverse
##              cached, the return value will be NULL.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<-y
    invmat <<- NULL
  }
  get <- function() {x}
  setinv <- function(inv) {invmat <<- inv}
  getinv <- function() {invmat}
  
  list(set = set, get = get, setinv = setinv, getinv =getinv)
}


## This function calculates the inverse of "matrix" x.
## If the inverse matrix is already cached, the cached data will be used. 
## Otherwise, the solve() function is called to calculate the inverse, and 
##   then cache the result.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse matrix.")
  } else {
    # no chache
    inv <- solve(x$get(), ...)
    x$setinv(inv)
  }
  
  return(inv)  
}
