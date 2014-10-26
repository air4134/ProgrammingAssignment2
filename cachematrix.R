## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##	makeCacheMatrix(x)
## This function creat a special matrix object (matrix wrapper)for a given matrix x. 
## This matrix object will cache its inverse result.
## The object has two attributes and four functions
## Attributes:
## 	x   - store the matrix, assumes it's a square matrix
##	inv - stores the cached inverse of the matric. It is null before any 
##	 	inverse is calculated calculation.
## functions 
##	set(y) - set the attrix attribute x to the new matrix (y)
##	get() - returns the matrix
##	setinv(a) - set the inverse (inv attribute) to a.
## 	getinv() - returns the inverse of the matrix  
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(a) inv <<- a
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  Short description #####
##  cacheSolve(x, ...)
## This function computes the inverse of a matrix, then caches the result.
## It calls the solve() function to get the inverse for the first time;
## caches the result. The result is reused after the first call of the function.
## The input is the special matrix object, a list and matrix wrapper.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	b <- x$getinv()
	if(!is.null(b)) {
		## for debuging purpose only
		print("getting cached inverse")
		return(b)
	}
	data <- x$get()
	b <- solve(data)
	x$setinv(b)
	b
}
