## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## This pair of functions cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.
## It contains functions to
## set the values of a matrix
## get the values of a matrix
## set the values of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL ## Matrix!
        }
        get <- function() x
        set_inverse <- function(inv) m <<- inv
        get_inverse <- function() m
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        mat.data <- x$get()
        inverse <- solve(mat.data, ...)
        x$set_inverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        return (inverse)
        
}       