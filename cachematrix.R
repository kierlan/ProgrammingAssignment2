## These two functions allow for the creation of a special matrix object which (in addition to being a matrix) holds a cache of its inverse within it.
## If no inverse is cached within the object, by attempting to retrieve the cache, a matrix inverse is calculated and stored within the matrix object

## This function creates the special matrix object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}

## This function attempts to retrieve the cached inverse, given a special matrix object,
## Else it calculates the inverse and stores it for future use

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }