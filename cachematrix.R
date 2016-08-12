## The functions below create a matrix object that can cache its inverse,
## compute the inverse of a matrix, cache the result and retrieve the inverse from cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## set inverse result to NULL initially
	inv <- NULL
	## set original matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## get original matrix
	get <- function() x
	## set inverse matrix when calculated for the first time in cacheSolve
	setinverse <- function(inverse) inv <<- inverse
	## get inverse matrix
	getinverse <- function() inv
	## put everything in a list
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## get status of inverse of the original matrix
	inv <- x$getinverse()
	## return the inverse from cache if it has already been calculated 
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## calculate and return inverse matrix if not available from cache
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
