## The following function creates a special "matrix", 
## which is really a list containing a function to set and get the value
## of the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
 # initializing inverse variable "inv" to NULL
    inv <- NULL
  ##  set the value of the matrix
       set <- function(y) {
                x <<- y
                inv <<- NULL
        }
  ##  get the value of the matrix
      get <- function() x
  ##  set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
  ##  get the value of the inverse
        getinverse <- function() inv
  ## returns a list of the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Check if the inverse of the matrix is cached
     inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
         ## returns the inverse of the matrix
                return(inv)
        }
        ## In this case of not being cached, we retrieve the matrix and inverse it
        data <- x$get()
        ## compute the matrix's inverse
        inv <- solve(data, ...)
        ## set matrix's inverse
        x$setinverse(inv)
        ## return the inverse
        inv
}
