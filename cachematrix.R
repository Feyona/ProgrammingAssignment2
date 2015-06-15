## Together, functions will retrieve cached inverse of a non-singular matrix. If no value is cached, then solve()
#is used to determine the inverse of the given matrix.

# Creates a list of functions, to be called by cacheSolve.
# set sets inital values for the matrix and inverse, based on user input; get returns the user input matrix;
#setInv updates the value of inv once it has been solved; getInv returns the calculated inverse.

makeCacheMatrix <- function(x=matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)
        
}

# Returns the inverse of user input non-singular matrix.
# If inverse has been calculated, retrieves value cached from makeCacheMatrix. Otherwise, uses the user input
#matrix and solve() to determine the inverse, and caches result before returning.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                message("Getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv        
}
