## MRN starting assignment 2
## These functions work together to speed up solving a square matrix by caching the 
## solution and refering to it in the future
# two stage process based on example. assumes input will be a square matrix that can be 
# inversed

## z is our practice matrix
#z <- matrix(data = c(1,2,2,1),nrow=2,ncol=2)
#z <- c(1,2,2,1)

# make Cache matrix takes in a vector and turns it into a matrix

makeCacheMatrix <- function(x = numeric()) {
        # take input data from function argment, make a matrix
        x <- matrix(data = x, ncol = length(x)/2)
        # make a matrix to collect inverse matrix
        m <- NULL
        # function to set local 'x' and 'm' varibles as global variables
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # make a series of functions to 
        # apply function to data x
        get <- function() x
        # apply solve to matrix x and store it in matrix m
        setinverse <- function(solve) m <<- solve
        # recall the solution matrix these functions are not called here
        # they are called in function cachesolve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        # set inverse of x to m
        m <- x$getinverse()
        # check to see if m has be solved for, if so recall m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if not, solve for m
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}