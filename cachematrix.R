## These functions can be used to create a list of function 
## for a matrix which has functions to get and set the data,
## as well as get and set a cached inverse of the matrix.

## Creates a cached matrix version of the matrix passed
## to the function. If no matrix passed, an empty matrix
## is created.
## Creates functions:
### get: gets data of matrix
### set: sets data of matrix
### getInverse: gets cached inverse if exists
### setInverse: sets cached inverse
makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseData) i <<- inverseData
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Performs the solve function on the cached matrix x.
## If the solve has already been performed, returns cached
## result, otherwise calucates inverse of matrix and caches
## the result.
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  ## Calculate inverse
  i <- solve(data, ...)
  
  #cache calculated inverse
  x$setInverse(i)
  i
}
