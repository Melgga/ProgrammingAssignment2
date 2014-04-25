## The 2 functions compute the inverse of a squared matrix, if the inverse was
## already computed, then the cached value is reported

## Generates a vector of 4 functions that manage the information
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## uses the 4 functions to compute the inverse or report the cached value
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

##commands for testing
a<-matrix(c(0,1,2,4),2,2)
l4functions<-makeCacheMatrix(a)
cacheSolve(l4functions) #computes inverse of a
cacheSolve(l4functions) #now gives the cached inverse of a

b<-matrix(c(2,0,0,.25),2,2)
l4functions$set(b)  #assigns b to the 4 functions
cacheSolve(l4functions) #computes inverse of b
cacheSolve(l4functions) #now gives the cached inverse of b


