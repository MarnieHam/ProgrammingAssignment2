# One function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
# The other function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#         If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#         should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
#    if X is a square invertible matrix, then solve(X) returns its inverse.

## Assumption: the matrix supplies is always invertible

## this function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL  set <- function(y) {
                x <<- y    
                M <- NULL  
                }  
        get <- function() x  
        setmatrix <- function (matrix) M <<- matrix()  
        getmatrix <- function() M  
        list(set = set, get = get,       
        setmatrix = setmatrix,       
        getmatrix = getmatrix)
}

## Creates a function that solves for inverse of matrix x if matrix is not null, if matrix is null pulls inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'}
        MI <- x$getinv()     
        if (!is.null(MI)) {          
                message("getting cached data")          
                return(MI)     }     
        data <- x$get()     
        MI <- solve(data, ...)     
        x$setinv(MI)     
        MI
}
## END
