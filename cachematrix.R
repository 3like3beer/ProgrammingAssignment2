## Matrix inversion is usually a costly computation 
## So there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly 
## The following pair of functions cache the inverse of a matrix.

## Example :
## c = rbind(c(1, -1/4), c(-1/4, 1))
## d<-makeCacheMatrix(c) # wrap c
## cacheSolve(d) ## first call : actual computation
## cacheSolve(d) ## second call : acces cached value


## This function creates a special "matrix" wrapper object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the special "matrix" wrapped in x
  m <- x$getinverse()
  
  ## Test if cache exists
  if(!is.null(m)) {
    message("getting cached data")
    ## return cached inverse
    return(m)
  }
  ## Get actual matrix from x wrapper
  data <- x$get()
  ## Actual computation if inverse has not yet been computed
  m <- solve(data)
  ## Cache inverse for x special "matrix" 
  x$setinverse(m)
  ## return inverse
  m
}
