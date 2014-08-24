## This R script has a function to take in a matrix, compute and cache its 
## inverse. It has another function to check if the inverse has been cached 
## before it computes its inverse. 

## This function takes in a matrix and keep a "special" matrix object to cache
## its inverse. It returns a list of set, get, setinverse, getinverse functions.

makeCacheMatrix <- function(x = matrix()) {
      ## initialize the "special" matrix object, inverse
      inverse <- NULL
      
      ## function set to set x
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## function get to get x
      get <- function() x
      
      ## function setinverse to set inverse
      setinverse <- function(inverseValue) inverse <<- inverseValue
      
      ## function getinverse to get inverse
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated then the cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## retrieve inverse from makeCacheMatrix
      inverse <- x$getinverse()
      
      ## if inverse exist, put up a message and return inverse
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      ## if inverse does not exist, get the input matrix, compute the inverse,
      ## set the inverse and return the inverse
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
