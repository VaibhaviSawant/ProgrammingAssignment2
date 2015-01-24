# Two functions are defined: makeCacheMatrix & cacheSolve.
# Implementation will be cacheSolve(makeCacheMatrix(myMatrix)), this will return the inverse of Matrix.
# First time the cacheSolve matrix is called, it will compute the inverse; for the rest calls cached values are returned.
# Suppose the value of myMatix is changed,then the value is calculated again and cached.


makeCacheMatrix <- function(myMatrix = matrix()) {
  inv <- NULL
  set <- function(y) {    		# set the value of the matrix
    myMatrix <<- y
    inv <<- NULL
  }
  get <- function() myMatrix				# get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse	# set the value of inverse of the matrix
  getinverse <- function() inv			# get the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
#Argument to the cacheSolve function is a list. This list is created in makeCacheMatrix using List().

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {					#Check if cache value exists.
    message("getting cached data.")
    return(inv)	
  }
  matrixData <- x$get()
  inv <- solve(matrixData)				#solve command is used to calculate matrix inverse.
  x$setinverse(inv)					
  inv
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
