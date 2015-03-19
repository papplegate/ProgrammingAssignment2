# cachematrix.R
# Patrick Applegate, following examples by Roger Peng
# 18 March 2015
#
# Contains two functions, makeCacheMatrix and cacheSolve, that 
# calculate the inverse of a matrix and cache it if it doesn't
# already exist.  
# 
# Based partly on a discussion by Hussain Boxwala and Eric
# W. Johnson at 
# https://class.coursera.org/rprog-012/forum/thread?thread_id=229

# /----- function makeCacheMatrix -----/
# Creates a new environment for potential caching of the matrix
# inverse and defines four functions for setting and getting
# the original matrix and its inverse from the cache.  

makeCacheMatrix <- function(x = matrix()) {
	
	# When this function is called, clear the local
	# value of the variable m.  Note that the cached 
	# variables will appear in the environment 
	# associated with this function.  
	m <- NULL
	
	# Function for setting the matrix, before inversion,
	# in the cache (after running this function, x in
	# the environment associated with makeCacheMatrix
	# will contain the value originally passed as y
	# in the function call, and m in the same
	# environment will be erased)
	set.matrix <- function(y) {
		x <<- y
		m <<- NULL
	} # end function set.matrix
	
	# Function for retrieving the cached matrix.  
	get.matrix <- function() {
		x
	} # end function get.matrix
	
	# Function for setting the inverse m in the cache.  
	set.inv <- function(inv) {
		m <<- inv
	} # end function set.inv
	
	# Function for retrieving the inverse m from the 
	# cache.  
	get.inv <- function() {
		m
	} # end function get.inv
	
	# Return a list of functions.  
	list(set.matrix = set.matrix, get.matrix = get.matrix, set.inv = set.inv, get.inv = get.inv)
	
} # end function makeCacheMatrix


# /----- function cacheSolve -----/
# Returns the inverse of x, which must be a square, invertible
# matrix.  If the inverse doesn't already exist, calculate its
# value; if it does exist, get it from the cache (which is
# the environment associated with the makeCacheMatrix function).  

cacheSolve <- function(x, ...) {
	
    # Return a matrix that is the inverse of 'x'
    
    # Retrieve the cached matrix inverse and store it in m.  
    m <- x$get.inv()
	
	# If the cached matrix inverse exists (is not NULL), 
	# print a message and return the matrix inverse.  
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# Retrieve the cached matrix.  
	data <- x$get.matrix()
	
	# Calculate the inverse of the matrix.  
	m <- solve(a = data, ...)
	
	# Set the matrix inverse in the cache.  
	x$set.inv(m)
	
	# Return the inverse of the matrix.  
	return(m)
        
} # end function cacheSolve
