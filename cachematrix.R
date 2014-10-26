## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly  

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(data = matrix()) {
  m<-list()
  length(m) <- length(data)
  dim(m) <- dim(data)       ## Creation of a null matrix for caching Inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() data
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the  cachesolve  should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  p <- lapply(m, is.null)   ## Checking if a cached Inverse exists
  for(i in 1 : length(m)) {
    flag <- 0
    if(p[i] == FALSE) {
      flag <- 1
      break
    }
  }
  if(flag) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()       ## If no cache exists, computing Inverse
  m <- solve(data)
  x$setInverse(m)
  m
}
