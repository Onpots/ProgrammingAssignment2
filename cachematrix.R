## The makeCacheMatrix creates a 2x2 matrix and prints it
## (default values 1,2,3,4)

makeCacheMatrix <- function(a=1,b=2,c=3,d=4) {
    i <- NULL
    x <- matrix(c(a,b,c,d), nrow = 2, ncol = 2)
    print("This is your matrix:")
    print(x)
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## cachesolve: Finds the inverse of a matrix and prints it. If the inverse has already
## been calculated it retrieves the inverse from the cache.

cachesolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  print("This is the inverse:")
  print(i)
}