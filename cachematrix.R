# Coursera: R Programming 
# Assignment 2
# The two functions are for computing and caching 
# the inverse of a given matrix.


# This function creats a special "matrix" object
# that can cache its inverse with cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL #s is to be the inverse of the matrix x
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(sol) s <<- sol
  getSolve <- function() s
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


# This function computes the inverse of 
# the "matrix" given by makeCacheMatrix function,
# and if the inverse has been calculated, retrieve it from cache
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    # the inverse is lready calculated, retrieve it from cache
    message("getting cached data")
    return(s)    
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}

