## Two functions for creating and manipulating "CacheMatrix" objects 
##  


## makeCacheMatrix function creates object that holds a matrix and 4 functions 
##   for setting and getting the value of the stored matrix and its inverse
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse
##     getinverse the value of the mean

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



## cacheSolve function takes a "CacheMatrix" object as an argument,
##   and checks if the inverse has been computed.  If it has, 
##   it returns the cached inverse, otherwise it computes the inverse, 
##   stores it, and returns the inverse

cacheSolve <- function(x, ...) {
        ## 1. Check if inverse of 'x' already exists
        ##   if so, return it and report that the cached value is being returned
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## 2. Otherwise, compute inverse, cache it, then return value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
