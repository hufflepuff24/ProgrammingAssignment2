## MRN starting assignment 2
## Put comments here that give an overall description of what your
## functions do
# two stage process based on example. assumes input will be a square matrix that can be 
# inversed

## Write a short comment describing this function
z <- matrix(data = c(1,2,2,1),nrow=2,ncol=2)
z <- c(1,2,2,1)

makeCacheMatrix <- function(x = numeric()) {
        x <- matrix(data = x, ncol = length(x)/2)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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