## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creates special matrix which is a list of functions to get/set matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## ix i is the inverse of x
    ix <- NULL
    
    set <- function(y) {
        x <<- y
        # x has changed, ix is unknown, 'unset' it
        ix <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) ix <<- inverse
    
    getinverse <- function() ix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    
    ## if inverse is already cached, use it 
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    
    mx <- x$get()
    
    #create the identity matrix with the right dimensions
    #md <- diag(dim(mx)[1])
    
    #calculate the inverse, equivalent to solve(mx,md) 
    ix <- solve(mx)
    
    x$setinverse(ix)
    ix    
}
