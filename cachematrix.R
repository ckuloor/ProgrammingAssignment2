## This is the source file that has the code for functions that will create 
## a matrix and cache the inverse of the  matrix for later use


## method to create matrix that can cache its inverse. this method has nested methods to get
## and set matrix inverse using the R solve method
## this method will return a list containing those methods

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matinv <<- solve
  getinverse <- function() matinv
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}


## method to return the inverse of the given matrix.
## if the inverse is not yet calculated, the setInverse method of the 
## cache matrix will be called to calculate the inverse and cached for the
## successive calls

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inversed matrix data")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
