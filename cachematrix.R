##-------------------------------------------------------------------------------
##  @Author: Dipanjan Sarkar
##-------------------------------------------------------------------------------
##  This script consists of two functions namely,
##
##     1. makeCacheMatrix() - This function creates a special matrix
##                            object that can cache its inverse.
##     2. cacheSolve()      - This function computes inverse of the special
##                            matrix returned by 'makeCacheMatrix' above.
##-------------------------------------------------------------------------------
##  Key points: If the inverse has already been calculated and the matrix has
##              not changed, then the 'cacheSolve' function should retrieve the
##              inverse of the matrix from the cache.
##-------------------------------------------------------------------------------


##--------------------
## makeCacheMatrix()
##--------------------
## The following function creates a "special" matrix, which is actually a list
## consisting of several functions which perform the following operations,
##
##    1. set()    - sets the value of the input matrix.
##    2. get()    - gets the value of the input matrix.
##    3. setinv() - sets the value of the matrix inverse.
##    4. getinv() - gets the value of the matrix inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  
  # initializing the inverse matrix value to NULL
  inv <- NULL
  
  # sets the value of the matrix with new input
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL     # 'inv' becomes NULL since matrix data changes
  }
  
  # gets the value of the matrix
  get <- function(){
    x
  }
  
  # sets the inverse value of the matrix
  setinv <- function(i){
    inv <<- i
  }
  
  # gets the inverse value of the matrix
  getinv <- function(){
    inv
  }
  
  # returns a list containing all the above functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


##--------------------
## cacheSolve()
##--------------------
## The following function calculates the inverse of the "special" matrix, created by the
## 'makeCacheMatrix' funtion. However, it first checks to see if the inverse has been
## calculated already. If the inverse matrix is present, then the value is fetched from the
## cache and all other steps are skipped. Otherwise, tf the inverse value is NULL, it means 
## the value is not in the cache and the inverse of the "special" matrix is calculated first
## using the 'solve' function and the inverse matrix value is set in the cache using the 
## 'setinv' function.

cacheSolve <- function(x, ...) {
  
  # Checks if the inverse is already in the cache  
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # inverse value not cached so we get the data of the "special" matrix  
  data <- x$get()
  
  # computes the inverse of the input matrix
  inv <- solve(data, ...)
  
  # stores this new value of inverse in the cache
  x$setinv(inv)
  
  # returns the new inverse matrix
  inv
  
}
