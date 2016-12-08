## R Programming - Programming Assignment 2

## This function creates a special vector that caches the calculation of its inverse
## It does this by creating the object as a list with `set` and `get` functions,
## and some clever scoping magic from R
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## The inverse starts out as NULL
  
  ## By setting the object, I assign the new argument to be my stored value and
  ## reset my inverse calculation
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Returns my internal object
  get <- function() x
  
  ## Get and Set functions for the inverse, as above
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will calculate the inverse of the received square matrix, caching
## the result in the CacheMatrix object.
## In the cache already exists, it will simply return the cached result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  ## Calculating the inverse
  x$setInverse(inv)
  inv
}
