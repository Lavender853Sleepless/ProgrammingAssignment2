## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" object made by makeCacheMatrix.
## These two functions together can compute the inverse of any invertible square matrix
## and cache the result.

## Description: makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Arguments: x is any square invertible matrix
## Returns: a list of functions, including getMatrix() for getting x, setInverse(inverse) for
##          caching the inverse of the x, and getInverse() for getting the inverse of x from
##          cache.

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  
  getMatrix <- function() x
  
  setInverse <- function(inverse) inversedMatrix <<- inverse
  
  getInverse <- function() inversedMatrix
  
  list(getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Description: cacheSolve computes the inverse of the special "matrix" object made by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed, then cacheSolve 
## will return the inverse from the cache.
## Arguments: x is the special matrix object created by makeCacheMatrix function
## Returns: the inverse of the special matrix

cacheSolve <- function(x, ...) {
  
  inversedMatrix <- x$getInverse()
  
  if(is.null(inversedMatrix)) {
    originalMatrix <- x$getMatrix()
    inversedMatrix <- solve(originalMatrix)
    x$setInverse(inversedMatrix)
  }
  else
  { message("getting from cache") }
  
  inversedMatrix
}
