##
## This pair of functions, makeCacheMatrix and cacheSolve, are used to create
## and operate on a matrix representation to provide a defered calculation of
## the matrix inverse and to cache that calculation to prevent wasteful recalculation.
## 
## makeCacheMatrix creates the 'representation' as a list of four function closures which
## provide get and set access to the matrix and its inverse.
##
## cacheSolve takes the matrix representation as input and returns the inverse
## of the matrix, either calculating the inverse or returning the cached value.



## Write a short comment describing this function

makeCacheMatrix <- function(the_matrix = matrix()) {
	the_inverse <- NULL
	set <- function(a_matrix) {
		the_matrix <<- a_matrix
		the_inverse <<- NULL
	}
	get <- function() {
		the_matrix
	}
	setinverse <- function(inv) {
		the_inverse <<- inv
	}
	getinverse <- function() {
		the_inverse
	}
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	stored_inverse <- x$getinverse()
	if (!is.null(stored_inverse)) {
		message("getting cached data")
		return(stored_inverse)
	}
	stored_matrix <- x$get()
	solved_inverse <- solve(stored_matrix)
	x$setinverse(solved_inverse)
	solved_inverse
}


