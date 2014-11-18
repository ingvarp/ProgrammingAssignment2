## R Programming 
## Programming assignment #2

## makeCacheMatrix

## This function creates a list object that contains a matrix and a set of functions:
## get(), set(), getinverse(), and setinverse()
## fuction get() returns a predefined matrix
## function set() overwrites the predefined matrix()
## function getinverse() returns calculated value of the inverse matrix stored in a variable i
## function setinverse() stores calculated value of the inverse matrix in a variable i

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(i) i <<- i
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## This function calculates the inverse matrix and stores it in a variable i
## in case the value in getinverse() is not empty, it returns stored value

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}