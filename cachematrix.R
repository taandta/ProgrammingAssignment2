## When calculations are perfomed on a large amount of data,
## a great amount of resources are used, such as memory, storage
## retrieval, and cpu time.Instead of performing the same calculations
## multiple times, R allows the programmer to compute
## once and store the value to be used for a later time.  

## This function creates a matrix and stores it's inverse 
## in the cache.


makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## The function below checks if the inverse of the matrix was
## previously computed. If it was computed (not null), the 
## function skips the computation and returns the result. If  
## it was not computed (null), the calculation for the inverse 
## is done and put in the cache.

cacheSolve <- function(x, ...) { 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
