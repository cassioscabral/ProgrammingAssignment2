## Functions that implement a cache system for the inverse of a matrix

## This function will make possible to create a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
          x <<- y
          inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solved_matrix) inverse <<- solved_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## This function will try to get the inverse of a matrix from cache, otherwise will calculate and save the result

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
