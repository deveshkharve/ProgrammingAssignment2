## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
# setter function for value of x and inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
#getter function for the value of x
    get <- function() x
#setter function for inverse object
    setinverse <- function(inverse) inv <<- inverse
#getter function for inverse object
    getinverse <- function() inv
#get list of all the function with perdefined names in parameters
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
	    
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
  	# Check for the returend value of inverse matrix is not null
  	  if(!is.null(inv)) {
    	    message("getting cached data.")
	# return the value of inverse matrix in case it is not null
   	     return(inv)
 	  }
	# else get data from x
  	 data <- x$get()
	#solve the x for creating the inverse matrix
  	 inv <- solve(data, ...)
	#set computed result into setter function with make cache object
  	 x$setinverse(inv)
	#return inverse object
  	 inv
}
