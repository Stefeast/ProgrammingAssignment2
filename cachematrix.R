# To cache the inverse of a matrix rather than compute it repeatedly use the following functions.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(u = matrix()) {
  i <- NULL
  set <- function(v) {
    u <<- v
    i <<- NULL
  }
  get <- function() u
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The following function returns the inverse of the matrix. 
cacheSolve <- function(u, ...) {
  i <- u$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- u$get()
  i <- solve(data)
  u$setinverse(i)
  i
}
## Sample run:
u <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 = makeCacheMatrix(u)
mat2$get()
## No cache in the first run
cacheSolve(mat2)
## Retrieving from the cache in the second run
cacheSolve(mat2)
