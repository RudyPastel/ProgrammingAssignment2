#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(),...) {
        #Initially, no inverse is calculated
        inv <- NULL
        #Updates the matrix x with new value y and sets the previous inverse to NULL for "inexistant"
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Functions that returns the cached data x
        get <- function() x
        #Function that updates the caches inverse with new cahced value
        setinv <- function(inverse) inv <<- inverse
        #Function that returns the inverse
        getinv <- function() inv
        #Previously calculated functions are retuned with matrix x and its inverse cached.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #Retieves the current value of the inverse inv of matrix x
        inv <- x$getinv()
        #If the inverse inv is already cached, this value is returned
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #The matrix x is extracted from cache and then inversed. The cached value of inv is updated
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
