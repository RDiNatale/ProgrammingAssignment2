## The following functions allow you to save a matrix and cache its inverse

## This function allows you to stora a special matrix and cache its inverse.
# GetMatrix() - returns the matrix that is being cached
# SetInverse(x) - allows you to set the inverse of a matrix to NULL or x, where x must be of class 'matrix'. 
# GetInverse() - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  GetMatrix <- function() x
  SetInverse <- function(y)
  if(class(y) != "matrix" & class(y) != "NULL"){
    stop("The value 'y' must be a matrix\n")
  }  
  else{i <<- y}
  GetInverse <- function() i
  
  list(GetMatrix = GetMatrix, GetInverse = GetInverse, SetInverse = SetInverse)
}


## This function returns the inverse of the matrix that was saved with makeCacheMatrix() and the message 'displaying cached data'. 
## If the inverse has not been set with the previous function, cacheSolve() will compute the inverse and store it in the cache. 

cacheSolve <- function(x, ...) {     
  i <- x$GetInverse()
  if(!is.null(i)) {
    message("displaying cached data")
    i }
  matrix <- x$GetMatrix()
  i <- solve(matrix, ...)
  x$SetInverse(i)
  i
}
