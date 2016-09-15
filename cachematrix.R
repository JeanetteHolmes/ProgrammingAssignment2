## This is code for an assignment from the "R Programming" course on Coursera.
## This pair of functions illustrate the use R's rules for lexical scoping to work 
## as a cache for computationally intensive operations.  This is a simple case for
## matrix inversion.  The makeCacheMatrix creates an object containing the matrix,
## its inverse, and some functions including functions for inverting the matrix.  
## The key is that the matrix only needs to be inverted once (unless the contents
## change) and the inverse will be stored along with the matrix, allowing it to be
## retrieved without re-calculating it.


## This function creates an object containing the matrix that is passed in,
## its inverse (once it's computed the first time), and functions to access
## both the original matrix and the inverse.  Note that this function does *not*
## contain the logic to actually compute the inverse, only to store it.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function uses the makeCacheMatrix function and its resultant objects
## to access the inverse of matrices.  First it tries to get a cached inverse,
## and if that does not exists it computes the inverse and stores it in the
## object for the next time the inverse is needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
