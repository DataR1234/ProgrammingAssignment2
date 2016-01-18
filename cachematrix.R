## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a matrix that can cache it's inverse
## Very similar to the 'special vector' example given

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv <<-NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set= set, get= get, setInverse = setInverse, getInverse= getInverse)
}

##cacheSolve returns the inverse of a matrix, first checking whether the inverse has already been converted.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$getInverse(inv)
  inv
}