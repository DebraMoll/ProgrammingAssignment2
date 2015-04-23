## makeCacheMatrix takes a matrix as a parameter and caches this matrix for use 
## later in the same R session (environment).
## it calculates the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachesolve accepts a makeCacheMatrix argument and returns the inverse of the matrix
## that was supplied to makeCacheMatrix
## m1 <- matrix(c(2,8,4,1), 2,2)
## cm1 <- makeCacheMatrix(m1)
## cacheSolve(cm1) 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}
