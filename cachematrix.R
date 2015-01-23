##R Programming - Programming Assignment 2 
##Caching the Inverse of a Matrix: The following functions were created to cache the inverse of a matrix 
##so it doesn't have to be recomputed evrery time it's needed.


## makeCacheMatrix is a funtion that creates a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initialize the stored inverse value to NULL
        
        ##Set the agrument as the stored value
        set <- function(y) {
                x <<- y
                ## Reset Inverse to NULL since the matrix is assigned a value
                inv <<- NULL
        }
        ##Get's the value of the stored matrix
        getMatrix <- function () x
        
        ## Set the inverse of the stored Matrix
        setInverse <- function(solve) inv <<- solve
        
        ## Get Inverse of stored Matrix
        getInverse <- function() inv
        
        ## returns list of all named funtions
        list(set = set, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This funtion computes the Inverse of the matrix produced by the
##makeCacheMatrix funtion above or return the cached result if the cache already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Inverse not found so we get the matrix, 
        data <- x$getMatrix()
        
        ##find its inverse
        inv <- solve(data)
        ## inverse and store it into cache
        x$setInverse(inv)
        
        ## Return the value found
        inv
}
