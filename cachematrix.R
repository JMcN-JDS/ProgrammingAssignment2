## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        InvM <- NULL
        set <- function(y) {
                x <<- y
                InvM <<- NULL
                get <- function() x
                setmatrix <- function(solve) InvM <<- solve
                #The above is setting the values of the special matrix
                getmatrix <- function() InvM
                list(set = set, 
                     get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
                #The above is getting the values of the special matrix
        }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        InvM <- x$getmatrix()
        if(!is.null(InvM)) {
                message("getting cached data")
                return(InvM)
                #The above checks to see whether the matrix has been calculated, 
                #and if so, gets the matrix from the cache and skips the computation 
        }
        matrix <- x$get()
        InvM <- solve(matrix, ...) #Using the solve() function to find the inverse of the matrix
        x$setmatrix(InvM)
        InvM
        #The above sets and returns the values of the inverted matrix
}
