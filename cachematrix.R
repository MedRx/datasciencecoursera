## Lexical scoping and caching functions that may require long computation time.
## Use solve() to find the inverse of a matrix and cache it using a free floating variable.
#
## a function that creates a unique matrix object that can cache inputed matrices and its' inverse
    makeCacheMatrix <- function(x = matrix()) {
#
## sets the value of m to NULL (a default if cacheSolve hasn't been used yet)
    m <- NULL
## set the value of the matrix
    set <- function(y) {
#
## caches the inputed matrix so cacheSolve can later check if it has changed
    x <<- y
#
## sets the value of m (the inverse matrix if cacheSolve is used) to NULL
    m <<- NULL
    }
## get the value of the matrix
    get <- function() x
#
## set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
## get the inverse of the matrix
    getinverse <- function() m
## creates a list of the four functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }
#
## cacheSolve functions to calculate the inverse of 
## the unique matrix objects above returned by makeCacheMatrix.
## cacheSolve checks if the inverse matrix has been calculated.
## If so, it gets the inverse from the cache and skips the calculation.
## If not, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setinverse function further down.
    cacheSolve <- function(x, ...) {
#
## If an inverse of x has already been calculated this function returns a matrix
    m <- x$getinverse()
#
## check if cacheSolve has run before
    if(!is.null(m)) {
#
## check that the matrix hasn't changed. If it hasn't,
## then send msg 'getting cached matrix'
    if(x$setmatrix() == x$getmatrix())
    message("getting cached data")
#
## return the inverse 'm'
    return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    }
