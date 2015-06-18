## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function return an object with a matrix as input. The return object is then
## used to perform set/get operation on matrix, e.g. bv<- makeCacheMatrix(k).
## It create a matrix inverse via setinverse() with solve(x) function.
## It returns the inverse , e.g. bv$getinverse(). 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function() m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function create cache from existing matrix object, e.g. bc<-cacheSolve(bv),
## whereby the input is matrix object from makeCacheMatrix().
## If the input is cached previously, the existing value is returned, otherwise
## it calculates new inverse of matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
