## Put comments here that give an overall description of what your
## functions do
install.packages ("matlib")
library(matlib)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat.inv <- NULL
  set.mat <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  get.mat <- function() x
  setinverse <- function(Inverse) mat.inv <<- Inverse
  getinverse <- function() mat.inv
  list(set.mat = set.mat, get.mat = get.mat,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mat.inv <- x$getinverse()
  if(!is.null(mat.inv)) {
    message("getting cached data")
    return(mat.inv)
  }
  data <- x$get.mat()
  mat.inv <- solve(data, ...)
  x$setinverse(mat.inv)
  mat.inv
}


##Testing
# 3*3 matrix

mymatrix <- matrix( c(5, 1, 0,
                      3,-1, 2,
                      4, 0, -1), nrow=3, byrow=TRUE)

det(mymatrix)

Cachemat<- makeCacheMatrix (mymatrix) 
Cachemat$get.mat()
Cachemat$getinverse()

cacheSolve(Cachemat)
