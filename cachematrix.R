## R functions to cache potentially time-consuming inverse matrix computations.

makeCacheMatrix <- function(x = matrix()) { ## makeCacheMatrix creates a special "vector", 
    m <- NULL                                 ## which is really a list containing a function to: 
  set <- function(y) {                   #1. set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                     #2. get the value of the matrix
  setSolve <- function(solve) m <<- solve #3. set the value of the inverse
  getSolve <- function() m                #4. get the value of the inverse
  
  list(set = set, get = get, ##5. generate list
       setSolve = setSolve,
       getSolve = getSolve)
}

cacheSolve <- function(x, ...) { ## Calculates the inverse of the special "vector", if it was not claculated before. 
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m ## Return a matrix that is the inverse of 'x'
}
