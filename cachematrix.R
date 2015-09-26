## Put comments here that give an overall description of what your
## functions do

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
        the_inverse <- x$getinverse()
        if (!is.null(the_inverse)) {
        	message("getting cached data")
        	return(the_inverse)
        }
        the_matrix <- x$get()
        the_inverse <- solve(the_matrix)
        x$setinverse(the_inverse)
        the_inverse   
}
