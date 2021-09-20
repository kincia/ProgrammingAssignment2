makeCacheMatrix <- function(x = matrix()) {
  j <- NULL ##set the value of the vector
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x ##get the value of the vector
  setInverse <- function(inverse) j <<- inverse ##set the value of inverse
  getInverse <- function() j ##get the value of inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}