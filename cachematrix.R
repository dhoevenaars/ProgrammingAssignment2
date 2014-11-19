# 'makeCacheMatrix' creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setfunction <- function(y){ 
                x <<- y
                inv <<- NULL
        }
        
        getfunction <- function(){ 
                x
        } 
        
        setinversefunction <- function(inverse){  
                inv <<- inverse 
        } 
        
        getinversefunction <- function() { 
                inv
        } 
        
        list(set = setfunction, get = getfunction, 
             setinverse = setinversefunction,  
             getinverse = getinversefunction)  
        
}

# 'cacheSolve' returns the inverse of the matrix created with makeCacheMatrix. It first 
# checks if the inverse has already been computed. If so, it gets the result and skips 
# the computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        inv
}