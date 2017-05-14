## Pair of functions which respectively create and calculate the inverse of a 
## matrix. The inverse of the matrix is cached by taking advantage of R's 
## scoping rules, which avoids repeating the calculations.

## This function creates a special matrix object, containing a list of functions 
## which respectively set the value of the matrix, get the value of the matrix, 
## set the matrix inverse and get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <-- y
        i <-- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function retrieves the cached inverse of the matrix created by the
## previous function or calculates the inverse if it has not been cached
## previously.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data = x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
