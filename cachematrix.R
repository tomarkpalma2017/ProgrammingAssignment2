## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### The following function creates a special "matrix " object which observes these steps:
####1. set the value of the matrix
####2. set the value the matrix
####3. set the value of the inverse
####4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
                    j <- NULL
                    set <- function(y){
                                        x <<- y
                                        j <<- NULL
                    }
                    get <- function()x
                    setInverse <- function(inverse) j <<- inverse
                    getInverse <- function() j 
                    list(set = set, get = get, 
                         setInverse = setInverse, 
                         getInverse = getInverse)
}


## Write a short comment describing this function
###cahceSolve determines the inverse of the special matrix returned by makeCacheMatrix. 
###Like the sample function cahemean, cahesolve intends to solve the inverse of the special matrix generated from the 
###function makeCahceMatrix whenever the inverse of the latter is not yet determined in the same function. 
###If so, the former function will just retrieve the inverse calculated from makeCacheMatrix.

cacheSolve <- function(x, ...) {
                    ## Return a matrix that is the inverse of 'x'
                    j <- x$getInverse()
                    if(!is.null(j)){
                                        message("getting cached data")
                                        return(j)
                    }
                    mat <- x$get()
                    j <- solve(mat,...)
                    x$setInverse(j)
                    j
}

r <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(r)