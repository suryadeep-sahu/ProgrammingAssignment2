## The functions cache the inverse of a square matrix.  Note the matrix must be square for the functions to work

## makeCacheMatrix creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix from the special matrix created by makeCacheMatrix 
## unless it has already been cached, in which case, the cached inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
