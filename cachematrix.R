## The 2 functions cache the inverse of a matrix and hence increase the speed in 
## working with large data matrices.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y                            #Assigns the value of x and i in the 
        i <<- NULL                         #global environment.
    }
    get <- function() x
    setsolve <- function(solveinverse) i <<- solveinverse
    getsolve <- function() i
    list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the matrix object
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if (!is.null(i)) {                     #Check if the value is already present                     
        message("getting cached data")     #in the cache.
        return(i)
    }
    data <- x$get()                        #Get the data and compute the inverse if 
    i <- solve(data, ...)                  #inverse is not present in cache.                
    x$setsolve(i)
    i
}


# Code Test Sample
# > x
# [, 1][, 2][, 3]
# [1,] 1 2 9
# [2,] 3 5 8
# [3,] 4 6 10
# > y <- makeCacheMatrix(x)
# > cacheSolve(y)
#           [, 1]       [, 2]       [, 3]
# [1,] - 0.1666667 - 2.8333333 2.41666667
# [2,] - 0.1666667 2.1666667 - 1.58333333
# [3,] 0.1666667 - 0.1666667 0.08333333
# > cacheSolve(y)
# Getting cached data
#           [, 1]       [, 2]       [, 3]
# [1,] - 0.1666667  -2.8333333 2.41666667
# [2,] - 0.1666667   2.1666667 -1.58333333
# [3,]   0.1666667  -0.1666667 0.08333333