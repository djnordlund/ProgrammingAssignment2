## Two functions for creating and manipulating "CacheMatrix" objects 
##  

##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse
##     getinverse the value of the mean

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Check if inverse of 'x' already exists
        ##   if so, return it
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Otherwise, compute inverse, cache it, then return value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
