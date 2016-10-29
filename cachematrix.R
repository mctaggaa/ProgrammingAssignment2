##
## Two functions for creating and computing  a matrix object which can cache its inverse
##
## makeCacheMatrix creates a special matrix object that can cache its inverse
##
## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix.  
## If the inverse has already been calculated and the matrix has not been changed, cacheSolve retrieves the inverse from the cache.


## makeCacheMatrix returns a special matrix object that can cache its inverse
##
## Input:	a matrix
## Returns:	a list containing the following functions as elements
##              set(x)
##              get()
##              setinverse(y)
##              getinverse()
##

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL


        set <- function(y) {

                x <<- y

                m <<- NULL

        }


        get <- function() x


        setinverse <- function(inv) m <<- inv

        getinverse <- function() m

        
        list ( set        = set
             , get        = get
             , setinverse = setinverse
             ,
 getinverse = getinverse)


}

## cacheSolve operates on the special matrix object created by the makeCacheMatrix function.
## It returns the inverse of the matrix used as input to makeCacheMatrix.  If the inverse matrix is cached,
## it returns the inverse from cache, otherwise it calculates the inverse.
##
## Input:	an object created by makeCacheMatrix
## Returns:	a matrix that is the inverse of the matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()


        if(!is.null(m)) {

                message("getting cached data")

                return(m)

        }


        data <- x$get()


        m <- solve(data, ...)


        x$setinverse(m)


        m


}
