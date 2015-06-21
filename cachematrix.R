## The functions work just as the ones in the example, except that the 
## first one takes a matrix as an input and that the second calculates
## the inverse (or not...).

## This first function (makeCacheMatrix) gives back a list, which provides
## the functions to change the matrix we have stored or to change the 
## inverse matrix we have stored, regardlessly if it is the true inverse
## matrix or not. 

makeCacheMatrix <- function(x = matrix()) {

	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This second function takes a list as input and gives back a matrix 
## as output. The given matrix is either the cached inverse or, if the
## m in "makeCacheMatrix" is still NULL (which means that our global 
## matrix has been changed), it is calculated again. 

cacheSolve <- function(x, ...) {
       
	  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
