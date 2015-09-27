##
##	This pair of functions, makeCacheMatrix and cacheSolve, are used to create
##	and operate on a matrix representation to provide a defered calculation of
##	the matrix inverse and to cache that calculation to prevent wasteful recalculation.
##	
##	makeCacheMatrix creates the 'representation' as a list of four function closures which
##	provide get and set access to the matrix and its inverse.
##
##	cacheSolve takes the matrix representation as input and returns the inverse
##	of the matrix, either calculating the inverse or returning the cached value.
##
##				October, 2015 -- Tom Cardozo
##


##	makeCacheMatrix()
##
##	This function creates the list of four function closures that provide the wrapper
##	for the matrix and its inverse.  The two variables, the_matrix and the_inverse,
##	are local variable in this function.  Because of lexical scoping, they are in the
##	environment wrapped in the closures, since thay are part of the defining environment.
##	The <<- assignment operator is used to set them from the set functions, since
##	otherwise they would just be creating local variables which would not to available
##	to the other functions.
##
##	Note that the environment has reference semantics -- it is changed in place,
##	without creating a new environment object. So, each function closure has
##	access to the same environment
##	
makeCacheMatrix <- function(the_matrix = matrix()) {
	the_inverse <- NULL
	
	#	Initialize the two variables
	#
	set <- function(a_matrix) {
		the_matrix <<- a_matrix
		the_inverse <<- NULL
	}
	
	#	Getter for the_matrix
	#
	get <- function() {
		the_matrix
	}
	
	#	Set the value of the_inverse after it has been calculated
	#
	setinverse <- function(inv) {
		the_inverse <<- inv
	}
	
	#	Getter for the_inverse
	#
	getinverse <- function() {
		the_inverse
	}
	
	#	Return the list of function closures
	#
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}

##	cacheSolve()
##
##	This function returns the inverse of the input matrix.
##	It returns the cached inverse if it has already been calculated,
##	otherwise it calculates the inverse, saves it and returns the inverse.
##
cacheSolve <- function(x, ...) {
	##	Return a matrix that is the inverse of 'x'
	##	'x" is actually the list of closures that represents the matrix

	#	Get the stored value of the inverse matrix
	#
	stored_inverse <- x$getinverse()

	#	If the stored value is not NULL, it's the cached inverse so return it
	#
	if (!is.null(stored_inverse)) {
		message("Getting cached inverse matrix.")
		return(stored_inverse)
	}
	#	Got Null stored_inverse: need to calculate the inverse

	#	Get the stored matrix, solve for the inverse, and store the result
	stored_matrix <- x$get()
	solved_inverse <- solve(stored_matrix)
	x$setinverse(solved_inverse)

	message("Returning calculated inverse matrix.")
	solved_inverse
}


##	sanitycheck()
##
##	This function is a quick test to verify that things are not broken
##
sanitycheck <- function() {
	mm <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 7), 3, 3)
	print("The test matrix:")
	print(mm)
	
	#	Next two lines are equivalent to:
	#		cmm <- makeCacheMatrix(mm)
	#
	cmm <- makeCacheMatrix()
	cmm$set(mm)

	print("The calculated inverse of test matrix:")
	print(cacheSolve(cmm))			# should be inverse matrix
	
	print("Expect: message, then the cached inverse of test matrix:")
	print(cacheSolve(cmm))			# should be inverse matrix preceded by message
}
