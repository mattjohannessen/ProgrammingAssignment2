## The following two functions allow for quick and easy inversion of a matrix using cacheing.

## makeCacheMatrix creates a matrix with the capability to cache it's inverse using the "solve" function.
## The function includes methods to set the value of the matrix, get the value of the matrix, set the inverse,
## and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

## The cachSolve acutally computes the matrix which can be returned by the makeCacheMatrix function 
## using pre-defined methods.

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
