## Put comments here that give an overall description of what your
## functions do

#### These functions are designed to invert matrices and cache the inverted 
#### matrix, so that it has to be calculated only once.

## makeCacheMatrix creates an environment for the solve function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inversematrix) invm <<- inversematrix
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## cacheSolve checks whether there is already an inverted matrix in Cache.
## If yes, it returns it. If no, it calculates it and returns it.

cacheSolve <- function(x, ...) {
  im <- x$getinvm() 
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinvm(im)
  ## Return a matrix that is the inverse of 'x'
  im
        
        
}
