## Two functions here can use to get matrix inverse,
## since it is s costly computation, an inverse calculated
## can be cached for future use

# This function creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, 
		 setinverse=setinverse, 
		 getinverse=getinverse)
}


## This function will provide the inverse. Then the inverse is cached.
## Cached matrix will be provided instead of calculating it again if called
## with the same matrix

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data.")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
