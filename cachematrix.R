## This code file contains a set of functions that can be used for creating a cacheable
## matrix inverse
## The inverse is cached in the parent environment
## The functions in this code file assume that the given input to create the cacheable matrix 
## will always be an invertible matrix

## Sample usage:
## NOTE: Please make sure that you have sourced cachematrix.R into the current working environment
## > te <- makeCacheMatrix(x = matrix(1:4, nrow = 2, ncol = 2))
## > cacheSolve(te)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Another sample usage of a 3X3 matrix (note that not all square matrices are invertible)
## NOTE: Please make sure that you have sourced cachematrix.R into the current working environment
## > te2 <- makeCacheMatrix(x = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nrow = 3, ncol = 3))
## > cacheSolve(te2)
## [,1] [,2] [,3]
## [1,] 0.75  0.5 0.25
## [2,] 0.50  1.0 0.50
## [3,] 0.25  0.5 0.75
## > cacheSolve(te2)
## getting cached data
## [,1] [,2] [,3]
## [1,] 0.75  0.5 0.25
## [2,] 0.50  1.0 0.50
## [3,] 0.25  0.5 0.75

## This function returns a type that can be used to calculate and cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        ## m below acts like a cache, initialized to NULL to start with
        m <- NULL
        set <- function(y) {
                x <<- y
                ## any time we provide a new value, reset the cache, otherwise for every new value
                ## we would end up getting the cached result
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of an invertible matrix
## Gets the value from cache if one already exists
## The input is a cacheable matrix created using the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        ## if we get a non-null m, we got a cache-hit. Return that and end the execution
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## cache-miss, m was null, now we have to calculate and store the inverse
        data <- x$get()
        m <- solve(data, ...)
        ## cache the calculated inverse here
        x$setInverse(m)
        m
}
