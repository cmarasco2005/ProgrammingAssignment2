##FUNCTION: makeCacheMatrix.R 
## Author: cmarasco2005 (based on framework provided by Dr. Peng)
## Date: 10.24.14

##Explanation: 
##makeCacheMatrix.R is a function containing an initially matrix
##the function allows you to cache the inverse matrix information 
##you calculate when your second function is called (cacheSolve).
##You may then call this cached information for future use in order
##to avoid having to rerun a matrix inversion calculation every time
##you'd like the data.

##FUNCTION: cacheSolve.R 
## Author: cmarasco2005 (based on framework provided by Dr. Peng)
## Date: 10.24.14

##Explanation: 
##cacheSolve.R is a function which will pull previously cached information,
##from a matrix and calculate it's inverse, then cache the new inverse
##matrix data. If there is previoulsy cached matrix data this function
##returns the message "getting cached data" and continues the calculation.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

