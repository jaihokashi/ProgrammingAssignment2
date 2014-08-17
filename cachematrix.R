## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        iMatrix <- NULL
        set <- function(y) {
                x <<- y
                iMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(mean) iMatrix <<- mean
        getMatrix <- function() iMatrix
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mx <- x$getMatrix()
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$setMatrix(mx)
        mx
}
