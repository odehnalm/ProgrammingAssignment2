##  Caching the Inverse of a Matrix

##  Computing the inverse of a matrix has always been costful, even though there are
##  modern techniques that make it quicker. If we have to calculate the inverse of a matrix,
##  then the best is to cache it, so we can just access the memory to reach it.
##  There are two functions involved in this task:
#   1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#   If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
#   the inverse from the cache.

##

## The following function creates a list containing a function to:
#    1. 'set' the value of the 'matrix'
#    2. 'get' the value of the 'matrix'
#    3. 'set' the value of its 'inverse'
#    4. 'get' the value of its 'inverse'

makeCacheMatrix <- function(X = matrix()) {
  X_inv <- NULL
  
  set_mat <- function(Y) {
    X <<- Y
    X_inv <<- NULL
  }
  
  get_mat <- function() X

  set_inv <- function(solve) X_inv <<- solve
  
  get_inv <- function() X_inv
  
  list(set_mat=set_mat, get_mat=get_mat, set_inv=set_inv, get_inv = get_inv)
}


## The following function takes the list (of functions) given by makeCacheMatrix() and returns the
## inverse matrix of 'X'. The function will compute the inverse but it will also store it in cache,
## so there will be no need to calculate it repeatedly.

cacheSolve <- function(L, ...) {

  X_inv <- L$get_inv()
  
  # Let's see if the inverse is already stored
  if(!is.null(X_inv)) {
    message("getting cached data")
    return(X_inv)
  }
  
  data <- L$get_mat()
  X_inv <- solve(data)
  L$set_inv(X_inv)
  X_inv
}

## RECOMMENDED FOT TESTING: 
# A<-matrix(rnorm(9),3,3)
# L<-makeCacheMatrix(A)
# A_inv<-cacheSolve(L)
# all(solve(A)==A_inv)
