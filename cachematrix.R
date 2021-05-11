## This file contains Matrix model and a helper function that will returns the matrix invers value, 
## computing it only once,then the cached value will be returned, for an effective usage.


## Constructs a matrix model, with the possibilty to set its inverse. 

MatrixModel <- function(mat=matrix()) {
        
        inv <- NULL
        
        set <- function(matrix) mat <<- matrix 
        get <- function() mat
        
        setInvers <- function(invers) inv <<- invers
        getInvers <- function() inv
        
        list(set = set , get = get , setInvers = setInvers , getInvers = getInvers )
}


## Returns the invers value of a given MatrixModel, the invers will be calculated only if it was not previously set, otherwise it will return the cached value

cacheSolve <- function(x, ...) {
        inv <- x$getInvers() 
        if ( !is.null(inv) ){
                # inverse value is found, returning it.
                message('printing cached result')
                return(inv)
        } 
        # calculating the inverse only if it was not previously set.
        # IMPORTANT : using solve will only calculate the inverse of a square matrix.
        inv = solve(x$get())
        x$setInvers(inv)
        return(inv)
}





