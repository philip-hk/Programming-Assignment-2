makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 # initially set to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x         # get the matrix
  setinverse <- function(inverse) inv <<- inverse      # set the inverse
  getinverse <- function() inv                         # get the inverse
  list(set = set, get = get,                           # group into a list
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve() compute the inverse of the matrix returned by makeCacheMatrix()
# or retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()        
  if(!is.null(inv)) {         # check if inverse is available or not
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
