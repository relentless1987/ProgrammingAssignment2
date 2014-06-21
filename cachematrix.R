## makeCacheMatrix:  creates a special matrix object that allows an inverse function
## cacheSolve: calculates and returns inverse of matrix or returns cached version of inverse if no changes

## create special matrix object from matrix input
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}


## Return inverse of makeCacheMatrix object from cache or after calculating using solve()
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
