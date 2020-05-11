## This finction helps us find the inverse of a matrix which can be a tedious task.
##By chacing the matrix we can avoid computing it recursively to find the inverse.

## makeCacheMatrix is a function that gives us a chached matrix, creating one from the square inversible matrix we have supplied. 
##It Sets the matrix, gets the matrix, sets the inverse, gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## cacheSolve caluclates the inverse of the cached matrix created above. 
##It checks if the matrix has been inverted, if yes then it gets that inverse and skips computation or else it computes the inverse using setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
