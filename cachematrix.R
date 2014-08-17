## Put comments here that give an overall description of what your
## functions do
# Two functions below create an object that stores the matrix and cache its inverse.

## Write a short comment describing this function
# The function makeCacheMatrix creates an object (a list) containing 4 functions that do the following:
# 1. set value of a matrix
# 2. get value of a matrix
# 3. set value of inverse matrix and
# 4. get value of inverse matrix
# The argument used in makeCacheMatrix function must be in the form of matrix object,
# for example makeCacheMatrix(matrix(c(1,2,2,1),2,2)) or makeCacheMatrix(rbind(c(1,2),c(1,2)))
# Use ?matrix in R to get more information on matrix objects.
# You can create an object/list with makeCacheMatrix function by assigning function's value to this object,
# for example: a <- makeCacheMatrix(matrix(c(1,2,2,1),2,2))
# Then you can refer to 4 mentioned earlier functions by running commands:
# Ad.1. a$get()
# Ad.2. a$set(matrix(c(0,0,0,0),2,2)) - argument inside brackets is just an example
# Ad.3. a$getinverse()
# Ad.4. a$setinverse(matrix(c(0,0,0,0),2,2)) - argument inside brackets is just an example
# Notes:
# a) a$getinverse() requires earlier finding of the inverse of matrix. It is NULL until cacheSolve() is run.
# b) only square matrices (n rows x n columns) are invertible. That comes from math.
# c) if you set the inverse matrix with setinverse() function then your correct result calculated with cacheSolve may be corrupted.
# d) <<- operator allows to assign a value to an object in an environment that is different from the current environment.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverseMatrix) m <<- inverseMatrix
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## Write a short comment describing this function
# The function cacheSolve calculates the inverse of the matrix created with makeCacheMatrix function.
# It checks first if the inverse matrix has already been calculated. If so then it gets the inverse from cache and skips the computation.
# In other case it calculates the inverse matrix and sets the value of inverse in the cache via setinverse function.
# Example of correct execution of the function for object "a" created using makeCacheMatrix would be: cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
	       message("getting cached data")
	       return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
