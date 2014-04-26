## Put comments here that give an overall description of what your
## functions do

# makeCacheMarix : returns a list of functions as..
## Set & Get the value of the matrix
## Set & Get the value of the inverse matrix

makeCacheMatrix<- function(x = matrix()) {
  ## the cached inverse matrix will be stored in "inv"
  inv <- NULL
  
  ## setting the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## getting the value of the matrix
  get <- function() x
  
  ## setting the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## getting the inverse of the matrix
  getinverse <- function() inv
  
  ## returning the defined functions as a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve : returns the inverse matrix computed, but if arleady there exist 
# the inverse then returns the cached inverse directly without useless computing..
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## if there exist the inverse arleady computed, then return it
  if(!is.null(inv)) {
    message("getting cached inverse Matrix")
    return(inv)
  }
  
  ## The inverse not computed yet, then solve it
  mdata <- x$get()
  inv <- solve(mdata, ...)
  
  ## cache inverse
  x$setinverse(inv)
  
  ## and returns it
  inv
}
