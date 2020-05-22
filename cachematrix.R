## Put comments here that give an overall description of what your
## functions do
install.packages ("matlib")
library(matlib)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  # define a function with a default mode of 'matrix'
  mat.inv <- NULL                            # set mat.inv as Null, not specified a default value, this will hold the matrix inverse
  set.mat <- function(y) {                   # define a function as set.mat as new
    x <<- y                                  # the operator <<- is only used in Functions, and cause a search to be made through parent environments for an existing definition of the variable being assigned 
    mat.inv <<- NULL                         
  }
  get.mat <- function() x                    # define the get.mat function which returns the matrix argument
  setinverse <- function(Inverse) mat.inv <<- Inverse  # set the value of inverse in parent environment
  getinverse <- function() mat.inv                     # get the value of inverse 
  list(set.mat = set.mat, get.mat = get.mat,          
       setinverse = setinverse(matrix()),
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mat.inv <- x$getinverse()                 
  if(!is.null(mat.inv)) {                     # if the inverse matrix is not null then you will get the message "getting cached data"
    message("getting cached data")           
    return(mat.inv)                           # return the inverse matrix - exit 
  }
  data <- x$get.mat()                         # if the inverse matrix is null then get the original inverse matrix
  mat.inv <- solve(data, ...)                 # solve(): to compute the inverse matrix 
  x$setinverse(mat.inv)                       # set the inverse matrix
  mat.inv                                     # print
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
