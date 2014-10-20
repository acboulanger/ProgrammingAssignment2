##Those functions that make it possible to cache potentially time-consuming computations, here the calculation of the inverse of a matrix.

##The first function, makecacheMatrix creates a special "matrix", which is a list containing a function to
##-set the value of the vector
##-get the value of the vector
##-set the value of the mean
##-get the value of the mean


makeCacheMatrix <- function(x = matrix()) 
{
		inv <- NULL
		setMatrix <- function(y)
		{
			x <<- y
			inv <<- NULL
		}
		getMatrix <- function() x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = setMatrix, get = getMatrix, setinv = setInverse, getinv = getInverse)
}

##The following function calculates the mean of the special "matrix" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setmean function.


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv))
	{
		return(inv)
	}
	else
	{
		matrix <- x$get()
		inv <- solve(matrix)
		x$setinv(inv)
		return(inv)
	}
}
