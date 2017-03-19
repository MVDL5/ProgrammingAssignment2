## Although there are functions that can perform matrix computations, it sometimes is time-consuming to recalculate certain values.
## Therefore, it may be worthwhile to cache values after caculations. Specifically, the following two functions calculate the inverse of a matrix
## and cache it to prevent the recalculation of it.


## The first function takes a matrix as an argument and assigns four functions to it:
## get() will return the matrix
## set(x) will change the matrix to x
## getinverse() will return the inverse of the matrix (if it has been calculated yet)
## setinverse(x) will change the inverse of the matrix to x

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The four functions assigned to the matrix in makeCacheMatrix above, will be used in cacheSolve to calculate the inverse of the matrix
## (if not yet been calculated) and then cache it. It will also return a matrix that is the inverse of the input matrix.

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
