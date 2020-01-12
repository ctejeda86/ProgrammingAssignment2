## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  #This allowed us to get the matrix and set it.
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#This function returns the inverse of the matrix, and evaluates if it has
#been calculated before, if it does, it skips its calculation.
cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
    x$setinv(inv)
  
  return(inv)
}


