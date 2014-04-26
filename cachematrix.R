## This file contains two functions which create a special object that stores 
##  a matrix (makeCacheMatrix) and cache's its inverse (cacheSolve)

## The first function, makeCacheMatrix creates a special "matrix" object, which 
## is indeed a list containing the following functions:
##
## 1. set the value of the matrix: set()
## 2. get the value of the matrix: get()
## 3. set the value of the inverse matrix: setinverse()
## 4. get the value of the inverse matrix: getinverse()

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
         getinverse = getinverse) # return a list of 4 functions
}


## The following function, cacheSolve, calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the matrix and sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv ## Return a matrix that is the inverse of 'x'       
}
