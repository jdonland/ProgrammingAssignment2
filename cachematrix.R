## Create an object which stores a matrix and caches its inverse.

## Create a list of functions to set/get the values of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i      <- NULL
  set    <- function(y)   {x <<- y ; i <<- NULL}
  get    <- function()    {x}
  setinv <- function(inv) {i <<- inv}
  getinv <- function()    {i}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Retrieve precomputed inverse if it's cached, otherwise compute inverse and cache it.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {message("getting cached data") ; return (i)}
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i  
}