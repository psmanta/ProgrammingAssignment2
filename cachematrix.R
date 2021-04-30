## The following two functions are meant to create and retrieve a cache as necessary for processing the costly matrix inversion function.
## Rather than calculating the inverse every time, we create an object of type makeCacheMatrix (explained below) and cache the inverse so
## that subsequent calls to invert the matrix specific to that instance will return the cached matrix rather than recalculating it everytime.

## makeCacheMatrix function
## creates an object of makeCacheMatrix which is given a matrix, builds a set of functions and returns those functions in a list.
## These functions are: 
## set - Assigns the matrix to the parent environment and clears previous cache. The key is assigning to the parent environment so things are retained.
## get - Returns the value of the matrix passed in or set with the set method
## setinv - does not actually calculate the inverse. Just sets the value passed to it
## getinv - returns the inverted matrix
## list - this is the return value of a MakeCacheMatrix object which is the variables (assigned into the Parent) and the 4 functions above returned as a list

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(aMatrix) {
		x <<- aMatrix
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv

	list(set=set, get=get, setinv=setinv, getinv=getinv)



}


## cacheSolve is designed to calculate, populate and retrieve the inverse of the matrix stored within a makeCacheMatrix object instance
## the function only accepts an obkect of type makeCacheMatrix. The first thing it does is calls x$getinv to try and retrieve an inverse matrix
## if one is stored. If a cached inverse matrix exists, it is returned from the cache. If one does not exist, it is calculated and set with 
## x$setinv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

	## If we have a cached instance, return the previously calculated and cached inverse
	if(!is.null(inv)) {
		message("Returning cached data")
		return(inv)
	}

	## we did NOT have a cached instance so calculate the inverse
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv

}  
