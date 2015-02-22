## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Stores the the matrix passed to it and its inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() return(x)
  setinv <- function(minv) m <<- minv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)          
}


## Write a short comment describing this function
## Calculates inverse of a matrix or retrieves it from the cache if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse ...")
    return(m)
  }
  data <- x$get()
  message("calculating inverse ...")
  m <- solve(data, ...)  
  x$setinv(m)
  m  
}
