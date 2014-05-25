## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function sets/returns the matrix and inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inversedmatrix) m <<- inversedmatrix
    
    getinverse <- function() m
    
    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function
## The function checks if there is cached matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    ## If the inversed matrix is cached already, it takes the cached value 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the 'x' value and assign to 'data'
    data <- x$get()
    
    ## Inverse the 'data' matrix and assign to 'm'
    m <- solve(data, ...)
    
    ## Store the 'm' inversed matrix to 'x'
    x$setinverse(m)
    m
}
