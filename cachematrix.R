## These functions create a special "matrix-like" object for situations where
## we might want to get the inverse of the matrix repeatedly.  This object will
## cache the inverse when it is first calculated, and provide the cached value
## rather than re-computing the inverse on successive calls.



## makeCacheMatrix constructs our special object, which stores the matrix (and 
## leaves room for its inverse).  It provides four methods: get/set access the 
## original matrix, and getinv/setinv access the inverse.  Changing the original
## matrix (by calling "$set()") will reset the cache to NULL.

## EXAMPLES OF USAGE:
##      m = makeCacheMatrix(matrix(3:6, nrow=2, ncol=2))
##      m$get()  <-- returns the matrix
##      m$set(matrix(1:4, nrow=2, ncol=2))   <-- sets the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    ## Don't calling these directly to obtain the inverse: call "cacheSolve" instead.
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve allows you to access the inverse of your original matrix.  If this is the
## first time it has been called since you last set the value of your matrix, it will 
## calculate it using solve().  If not, it will simply retrieve the cached value, which
## speeds things up considerably.

## EXAMPLES OF USAGE:
##      cacheSolve(m)   <-- returns the inverse of a matrix created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
