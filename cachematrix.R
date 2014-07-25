## R script to compute the inverse of a square matrix using caching

##Function to create 'special' matrix for Matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL                           ## 'special' matrix 
    get <- function() x
    setinv <- function(inv) invMatrix <<- inv
    getinv <- function() invMatrix
    list(get = get,
         setinv = setinv,
         getinv = getinv)                   
}


## Function to return the inverse of a square matrix, computes value if 
## no cached version exists

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinv()
    if(!is.null(invMatrix)) {               ## checks if a cached value exists
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)           ## calculates inverse of the matrix
    x$setinv(invMatrix)
    invMatrix                       ## Returns a matrix that is the inverse of 'x'
}
