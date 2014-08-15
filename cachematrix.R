## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix <- function(x = matrix()) creates a special
## matrix x, which is really a list containing functions to :
## 1. set the value of a matrix x and its inverse to NULL into the cache
## 2. get the value of a matrix x from cache
## 3. set the value of the inverse of the matrix into the cache
## 4. get the value of the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## This function initializes x to a fresh copy of the matrix y
        ## and x's inverse to NULL in the underlying enviroment or cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## returns x from the cache
        get <- function() x
        
        ## sets the inverse of matrix x (i.e. inv variable) in the cache
        setinverse <- function(inverse) inv <<- inverse
        
        ## outputs the inverse of matrix x from the cache
        getinverse <- function() inv
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The function cacheSolve <- function(x, ...) outputs
## the inverse of the given matrix x. This function first checks to see whether 
## any cached value of the inverse of matrix x persists in the environment. If 
## found, it returns the cahced value with a message. Otherwise, it computes the 
## inverse, caches it into the environment and outputs the inverse of x.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
        ## extracts the cached value of the inverse of matrix x from the 
        ## environment
        inv <- x$getinverse()
        
        ## if the cached value is found (i.e., not null), 
        ## return the inverse of x with a message
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the cached value is null, compute the inverse and 
        ## cache it into the environment
        
        ## get the matrix x and assign it to the variable data
        data <- x$get()
        ## compute the inverse of matrix data and assign it to inv
        inv <- solve(data)
        ## cache inv into the environment
        x$setinverse(inv)
        ## return inverse of the given matrix x
        inv
}
