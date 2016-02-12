## These functions either compute the inverse of a matrix and caches it or 
## retrieves the value from the cache

## This function gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- mean
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}