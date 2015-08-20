## Put comments here that give an overall description of what your
## functions do

##  The first function stores a set of four functions and returns a list
##  of those four functions.

##  The second function checks to see if a value has been calculated
##  provided an object that stores the first function. 
##  If that value has not already been calculated, 
##  it calculates the value (in this case a matrix, or inverse matrix
##  and then stores it. 

## Write a short comment describing this function

##  The first function stores a set of four functions and returns a list
##  of those four functions.
## The set function assigns variables in the parent environment
## the get function returns the original input 
## setinversion assigns the "mat" variable to be the inverted matrix
## getinversion prints the inverted matrix



makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinversion <- function(setinversion) mat <<- setinversion
    getinversion <- function() mat
    list(set = set, get = get, setinversion = setinversion, 
         getinversion = getinversion)
}


## Write a short comment describing this function

##  The second function checks to see if a value has been calculated
##  provided an object that stores the first function. 
##  If that value has not already been calculated, 
##  it calculates the value (in this case a matrix inversion)
##  and then stores it. 

cacheSolve <- function(x, ...) {
    
    mat <- x$getinversion()
    if(!is.null(mat))  {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data)
    x$setinversion(mat)
    mat
       ## Return a matrix that is the inverse of 'x'
}
