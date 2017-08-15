## PJP 14 Aug 17, Programming Assignment 2
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix1 <- NULL
  set <- function(y) {
    x <<- y
    matrix1 <- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) matrix1 <<- solve
  getinverse <- function() matrix1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function looks for a matrix inverse value in the cache and then calculates if not found
cacheInverse <- function(x, ...) {
  matrix1 <- x$getinverse()
  if(!is.null(matrix1)) {
    message("getting cached data")
    return(matrix1)
  }
  data1 <- x$get()
  matrix1 <- solve(data1, ...)
  x$setinverse(matrix1)
  matrix1
}

## test
matrix2 <- makeCacheMatrix(matrix(4:7, 2, 2)) # set matrix
cacheInverse(matrix2) # check cache, then calculate
cacheInverse(matrix2) # should return a cached value with message

matrix3 <- matrix(4:7, 2, 2) #test against a known function
solve(matrix3) # test function

matrix4 <- makeCacheMatrix(matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3)) # test on a higher order matrix
cacheInverse(matrix4) # check for value, then calculate
