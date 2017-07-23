## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL 
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x 
        setInv <- function(inverse) Inv <<- inverse
        getInv <- function() Inv
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- X$getInv()
        if(!is.null(Inv)) {
                message("getting cashed data")
                return(Inv)
        }
        x <- X$get()
        Inv <- solve(x)
        X$setInv(Inv)
        Inv
}
