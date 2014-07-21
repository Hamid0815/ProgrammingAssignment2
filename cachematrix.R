## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Returns a list of setter, getter functions for matrix and inverse values 
makeCacheMatrix <- function(x = matrix()) {
        
        # for storing the cache inv matrix
        invm <- NULL
        
        # Set the data in global memory
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        
        # get method
        get <- function(){ 
                x
        }
        
        # Setter for the inverse
        setinv <- function(inverse){ 
                invm <<- inverse
        }
        
        # Getter for the inverse
        getinv <- function(){ 
                invm
        }
        
        # Return functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Computes and return the inverse of a matrix. 
## First checks the global buffer, if no data found, computes
## the inverse of a matrix and passes it to the buffer function 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinv()
        
        # check if the inverse exists
        if (!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
        # Not in buffe--> calculate it
        data <- x$get()
        invm <- solve(data, ...)
        
        # Cache the inverse
        x$setinv(invm)
        
        # Return inverse
        invm
}

