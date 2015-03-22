# # This function creates a special "matrix" object
# # that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      # # creates null 'inv', stores 'x' as 'y' in the global environment, 
      # # also makes 'inv' global
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      # # retrieves the function storing x, solves/inverts, caches in inv in parent enviro
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# # Computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
# # If the inverse has already been calculated (and the matrix has not changed),
# # strthen `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      # # stores solved/inverse of 'x' in previously NULL 'inv'
      inv <- x$getinv()
      
      # # checks if there is already cached data/vector, in this case the matrix
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # # 'if' statement is not fullfiled, gets 'x', stores it as data, inverts, returns
      message("calculating data")
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}