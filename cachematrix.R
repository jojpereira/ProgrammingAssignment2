makeCacheMatrix <- function(x = matrix()) {

## Please note that it's necessary to fill the nrow and the ncol
##e.g makeCacheMatrix(matrix(3:6,2,2))
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve 
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cachesolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
##Return a matrix that is the inverse of 'x' and is a quadratic matrix
##with a possible inverse value
##Please note that not all the matrix have an inverse e.g (1:9, 3,3) returns 
##"Error in solve.default(data, ...) : 
##Lapack routine dgesv: system is exactly singular: U[3,3] = 0, which means 
##that there isn't an inverse matrix for 
##     [,1] [,2] [,3]
##[1,]    1    4    7
##[2,]    2    5    8
##[3,]    3    6    9
}
