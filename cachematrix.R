## R functions to illustrate 
## cache use

## This function creates a special "matrix" object that can cache its inverse.
## Functions of makeCacheMatrix
## set: set a invertible matrix
## get: get the invertible matrix  storage
## setsolve: put the inverse to the cache
## getsolve: retrieve the inverse from the cache
makeCacheMatrix <- function(matrixNotInversed = matrix()) {
  ## Initialize cache 
  matrixInversedCached <- NULL
  ## set a invertible matrix and reset cache to null
  set <- function(y) {
    matrixNotInversed <<- y
    matrixInversedCached <<- NULL
  }
  ## get a invertible matrix 
  get <- function() matrixNotInversed
  ## set a inverse matrix to cache
  setsolve <- function(matrixInversed) matrixInversedCached <<- matrixInversed
  ## retrieve a inverse matrix from cache
  getsolve <- function() matrixInversedCached
  ## return a vector of functions
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInversed <- x$getsolve()
  ## return matrixInversed, if It is not NULL, 
  if(!is.null(matrixInversed)) {
    message("getting Matrix Inversed cached data")
    return(matrixInversed)
  }
  ## Other case, computes the inverse of X and cached It.
  matrixNotInversed <- x$get()
  matrixInversed <- solve(matrixNotInversed)
  x$setsolve(matrixInversed)
  matrixInversed
}
