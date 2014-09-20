## create a special invertible "matrix" object, computes the inverse of
## the matrix and return the result, If the inverse has already been calculated
## (and the matrix has not changed), then just return the cached result


## function makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(A = matrix()) {
	#set variable r (the inverse of a matrix in this case) to NULL
	r <- NULL

	#set function sets A to the argument B and set r to null
	set <- function(B) {
		A <<- B
		r <<- NULL
	}

	#get function returns the value of A (argument of makeCacheMatrix)
	get <- function() A

	#sets r in makeCacheMatrix to solve (the inverse of the matrix)	
	setsolve <- function(solve) r <<- solve

	# getsolve returns the value of r (from makeCacheMatrix)
	getsolve <- function() r

	#returns a list containing functions set, get, setsolve and getsolve
	list(set = set, get = get,
	setsolve = setsolve,
	getsolve = getsolve) 

} 


## function cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the retrieve the cached inverse

cacheSolve <- function(A, ...) {
	#attempts to get the inverse of the matrix from A (if it was calculated previously)
	r <- A$getsolve()

	 #if not null, a valued was cached, so return r
	if(!is.null(r)) {
		message("getting cached data")
		return(r)
	}

	#since its null, set data to A from makeCacheMatrix
	data <- A$get()

	#calculate the inverse of the matrix
	r <- solve(data, ...)

	#set r in A to calculated inverse
	A$setsolve(r)

	#return the inverse 
	r
}
