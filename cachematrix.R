## makeCacheMatrix and cacheSolve are functions that when combined together can cache the inverse of a matrix in order
## to prevent multiple recalculation of the same value

## This function receives a matrix and returns a list of 4 functions that are: 
## 1) Store the original matrix in a different environment
## 2) Return the original matrix
## 3) Store the provided value as a result of inversing the original matrix
## 4) Return the original matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function receives an output of makeCacheMatrix function and returns the value of inversing 
## the original matrix supplied to it. If the value was already calculated - it is returned as is, skipping the calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
