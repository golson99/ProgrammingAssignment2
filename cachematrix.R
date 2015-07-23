##the two functions makeCacheMatrix and cachSolve provide the ability to pass in a matrix, cache the inverse of the matrix when
## a cached version doesn't already exist, and return the inverse of the matrix  



## makeCacheMatrix creates a matrix and list of functions to 
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##create functions to set/get the matrix and inverse of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachSolve checks to see whether a cached version of the inverse of a given matrix exists.  If so, the cached version is returned,
## otherwise the inverse is calculated and returned

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            ## Return the cached inverse of the matrix
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
    }
