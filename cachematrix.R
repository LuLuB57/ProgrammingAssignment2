## Coursera Data Science: Programming Assignment 2
## LuLuB57
## Cache Matrix Functions 
## 
## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse
## cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already ## been calculated (and the matrix has not change), 
## the cacheSolve should retrieve the inverse from the cache.



## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}
get <- function() x
setInverse <- function(inverse) inverseMatrix <<- inverse
getInverse <- function() inverseMatrix 
list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   		inverseMatrix <- x$getInverse()
   		if (!is.null(inverseMatrix)) {
   			message("getting cached data")
   			return(inverseMatrix)
   		}
   		matrix <- x$get()
   		inverseMatrix <- solve(matrix, ...)
   		x$setInverse(inverseMatrix)
   		return(inverseMatrix)
}
