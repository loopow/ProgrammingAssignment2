## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## Sample run:
## > x = matrix( 
##  +     c(1,3,2,4),
##  +     nrow=2,ncol=2)
## > i = makeCacheMatrix(x)
## > i$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(i)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > cacheSolve(i)
## getting cached data
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
