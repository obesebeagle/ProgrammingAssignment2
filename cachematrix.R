## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions do exactly that.

## The function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #this variable m is used to store the inverse matrix later
  set <- function(y) { 
    x <<- y #set the value of x to a new matrix y
    m <<- NULL #Reset the mean value when 'x' is changed
  }
  get <- function() x #returns the current value of x
  setinverse <- function(solve) m <<- solve #obtain the inverse. Assigns it to m
  getinverse <- function() m #returns the inverse matrix in m.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##the function cacheSolve() computes the inverse of the special matrix only if the inverse 
##has not yet been computed (m is empty), or the values of the special matrix has changed   

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
