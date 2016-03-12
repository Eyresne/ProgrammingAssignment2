## Stores the result of the function Solve() in a list so that it can be retrieved by cacheSolve later. MAy save time with large matrices.

### uses the function "Solve" to find the inverse of a matrix and stores that answer in a cache (in list format) that cachesolve can read.
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


## examines x - if x has a solution saved by makeCacheMatrix above, outputs that solution. 
## Otherwise runs solve() again and outputs the solution

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
