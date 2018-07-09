	## Assignment 2: The first code can cache the inverse of a matrix and the second function can solve this cache, computing the inverse.
	
	## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
	
	makeCacheMatrix <- function(ma = matrix()) {
	  mi <- NULL
	  set_ma <- function(y) {
	    ma <<- y
	    mi <<- NULL
	  }
	  getma <- function() ma
	  set_matrix_inverse <- function(matrix_inverse) mi <<- matrix_inverse
	  get_matrix_inverse <- function() mi
	  list(set_ma = set_ma, getma = getma, set_matrix_inverse = set_matrix_inverse, get_matrix_inverse = get_matrix_inverse) 
	}
	
	## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
	
	cacheSolve <- function(x, ...) {
	  mi <- x$get_matrix_inverse()
	  if(!is.null(mi)){
	    message("getting cached data")
	    return(mi)
	  }
	  data_ma <- x$getma()
	  mi <- solve(data_ma, ...)
	  x$set_matrix_inverse(mi)
	  mi
	}
