## Programming Assignment 2 
## R function to cache potentially time-consuming computations

## Function to to cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to compute the inverse of matrix  

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}

