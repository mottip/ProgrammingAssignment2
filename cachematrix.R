## a cache mechanism for the inverse of a matrix

## a container for a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## solves inverse for an object created with function makeCacheMatrix. 
## if uses precalculated inverse if available other wise
## calculates and caches the inverse

cacheSolve <- function(x, ...) {
  if (is.null(x$getinv())) {
    inv <- solve(x$get())
    print('caching solved inverse')
    x$setinv(inv)
  } else {
    print('using cached inverse')
    inv <- x$getinv()
  }
  inv
}

