# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# Here is a pair of functions that cache the inverse of a matrix and compute the
##inverse if not available in cache.


## makeCacheMatrix: Function to create a special object that stores a matrix
## and caches its inverse


makeCacheMatrix <- function(x=matrix()){
        
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv_mat <<- solve
        getsolve <- function() inv_mat
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve: Function to display the inverse of a matrix.
## The results is directly obtained from cache if already computed, else
## cacheSolve will compute the inverse of the matrix.


cacheSolve <- function(x,...) {
        inv_mat <- x$getsolve()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setsolve(inv_mat)
        inv_mat
}