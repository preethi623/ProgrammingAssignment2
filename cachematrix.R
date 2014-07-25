################################################################# 
## Functions to take a invertible matrix, compute inverse matrix
## check if inverse matrix is already cached for the given matrix 
## else generate the inverse matrix, cache it & return it.

## makeCacheMatrix function
## ------------------------
## this function is first called to create a list of functions 
## to generate and return given matrix & its inverse matrix
## it returns the list of functions 

## cacheSolve function
## --------------------
## this function is called next to generate inverse matrix 
## it takes list object and options to 'solve' function.
## it checks if inverse matrix exists,
## else generate one with solve function and returns the inverse matrix.

#################################################################


## makeCacheMatrix function
##--------------------------
## This function is used to create a list object with functions 
## to create & return given matrix and its inverse matrix
## takes one argument and returns a list object with functions 
## to set or get  given matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # creates a variable S 
	s <- NULL

	# function to set the values of matrix
        set <- function(y) {
	       x <<- y
	       s <<- NULL
	}

	# function to return the matrix
	get <- function() x

	# function to set the values of inverse matrix
        setsolve <- function(solve) s <<- solve

	# fucntion to return inverse matrix
        getsolve <- function() s
 
 	# returns list of functions to set , get matrix and its inverse matrix
        list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
      }
	

## cacheSolve function
## -------------------
## cacheSolve function takes list object and required solve funtion options if 
## necessary, checks if a inverse matrix is already cached and returns it.
## If inverse matrix does not exist, 
## gets the original matrix through list object get function,
## computes the inverse using solve function 
## caches the inverse matrix using setsolve function 
## and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	# function call to get inverse matrix associated with 'x' and stores as 	's'
	s <- x$getsolve()

	# returns 's'(inverse matrix) if 's' is not null
        if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}

	# get the matrix for which inverse matrix should be calculated and store
	#it as data
	data <- x$get()

	# computes inverse of matrix and stores as 's'
        s <- solve(data, ...)

	# caches inverse matrix 's'
	x$setsolve(s)

	# returns inverse matrix 's'
        s
										
}
