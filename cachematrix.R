## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse) i <<- inverse 
        getinv = function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by 
        ## makeCacheMatrix above. 
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then the cachesolve should retrieve the inverse from the cache
        
        i = x$getinv()
        
        if (!is.null(i)){
                message("Getting Cached Data")
                return(i)
        }
        
        mat.data = x$get()
        i = solve(mat.data, ...)
        
        x$setinv(i)
        
        return(i)
}