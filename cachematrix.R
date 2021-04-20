#=====================================================
# 
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly      
#
# The purpose of this function is to write a pair of functions that cache the
# inverse of a matrix. This pair of functions are:
#
#       1. makeCacheMatrix: 
#       2. cacheSolve:      
#
# Computing the inverse of a square matrix can be done with the solve function.
# For example, if X is a square invertible matrix, then solve(X) 
# returns its inverse.
#
# Assume that the matrix supplied is always invertible. 

makeCacheMatrix <- function(x = matrix()) {
        #        
        #  This function creates a matrix "x" that can cache its inverse.
        #                            
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m_inverse <<- inverse
        getinverse <- function() m_inverse
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

cacheSolve <- function(x, ...) {
        #    
        #  This function return a matrix that is the inverse of 'x' returned
        #  by makeCacheMatrix above.
        #        
        #  If the inverse has already been calculated (and the matrix has not changed), 
        #       then the cachesolve should retrieve the inverse from the cache and skip 
        #       the computation.        
        #  Otherwise, it calculates the inverse of the data and sets the value of the 
        #        inverse in the cache via the setinverse function.
        
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached inverse data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data, ...)
        x$setinverse(m_inverse)
        m_inverse
}