## Below are two functions that are used to create a special object that stores 
## a matrix and cache's the inverse of the matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to:
## 1. set the components of the matrix
## 2. get the components of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setmatinv <- function(minv) matinv <<- minv
        getmatinv <- function() matinv
        list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)
}


## The second function cacheSolve calculates the inverse of the special "matrix"
## created with the first function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the setmatinv 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getmatinv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        mat <- x$get()
        matinv <- solve(mat)
        x$setmatinv(matinv)
        matinv
}