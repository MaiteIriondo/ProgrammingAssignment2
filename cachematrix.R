###############################################################
####            ASSIGNMENT 2                            #######
###############################################################


##The function makeCacheMatrix creates a list of functions that set and get the value
#of a matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y= matrix()) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inv) inversematrix <<- inv
  getinversematrix <- function() inversematrix
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The following function calculates the inverse matrix of the matrix created with 
#the above function. However, it first checks to see if the inverse has already been
#calculated. If so, it gets the inverse matrix from the cache and skips the computation.
#Otherwise, it calculates the inverse matrix of the data and sets the value 
#of the inverse in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
  inversematrix <- x$getinversematrix()     
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinversematrix(inversematrix)
  inversematrix
}
