## cachematrix contains 2 functions: makeCacheMatrix & cacheSolve
##to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve creates an inverse of a special "matrix 
##created with above function
## First, it checks if the matrix has been inversed. 
## If so, it get the inversed matrix from the cache 
## and skips the computation. 
## Otherwise, it inversed the matrix and sets the inversed
## matrix in the cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
