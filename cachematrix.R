## Two functions that cache the inverse of a matrix
## Creates a special matrix object that caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) 
  {
    m <<- y
    i <<- NULL
  }
  
  get <- function() {m}
  
  setInverse <- function(inverse) 
  {
    i <<- inverse
  }
  
  getInverse <- function() {i}
  
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)

}


## Calculates the inverse of the special matrix returned by the above function.
##Checks if the inverse has already been calculated. If yes, then "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if( !is.null(m) ) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
