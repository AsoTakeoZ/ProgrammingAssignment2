## Programming Assignment 2 

## Makes an object that stores an origial matrix x and a solution matrix Inv

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


## Returns inverse of original matrix stored in X. Uses cached solution if 
## inverse for the same original matrix has already been calculated. 

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
