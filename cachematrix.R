## Functions to invert and cache the result of square matrices

## Create an object that will wrap the matrix

makeCacheMatrix <- function(x = matrix()) {
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


## function that inverts the matrix and caches the result for subsequent calls

cacheSolve <- function(x, ...) {
  
  ## make sur we are using a proper object
  if (class(x) != "list" || class(x$getInverse) != "function") {
    print ("x is not an object created by makeCacheMatrix")
    return(NULL)
  }
  

  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
