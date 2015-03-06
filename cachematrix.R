## Functions to invert and cache the result of square matrices

## Create an object that will wrap the matrix
## args:     x: the square matrix to be inverted
## returns:  a CacheMatrix object

makeCacheMatrix <- function(x = matrix(c(1.0,0.0,0.0,1.0),c(2,2))) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function that inverts the matrix created by makeCacheMatrix and caches the result for subsequent calls
## args:     x:   the CacheMatrix returned by makeCacheMatrix
##           ...: additional arguments to be passed to solve
## returns:  the inverted matrix if everything ok, NULL if the x argument is incorrect
cacheSolve <- function(x, ...) {
  
  ## make sur we are using a proper object
  if (class(x) != "list" 
      || class(x$get)   != "function"
      || class(x$get()) != "matrix") {
    print ("x is not a well formed object created by makeCacheMatrix")
    return(NULL)
  }
  
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
