## Creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    
    set <- function(y) {            
        x <<- y                     ## Reassign matrix to x 
        z <<- NULL                  ## Reinitialize z to NULL
    }
    
    get <- function() x
    setInvmatrix <- function(InvMatrix) z <<- InvMatrix
    getInvmatrix <- function() z
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    m <- x$getInvmatrix()              
    
    if(!is.null(m)) {           ## If user has calculated the same matrix before, get old result
        return(m)               
    }
    
    data <- x$get()             ## If user has not calcaulated matrix before (else part), get uncalculated matrix
    m <- solve(data, ...)       ## Get inverse matrix
    x$setInvmatrix(m)           ## Re-assign inverse matrix 
    m                           ## Print inverse matrix 
}
