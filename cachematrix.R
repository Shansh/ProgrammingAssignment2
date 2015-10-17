## Following functions are caching the inverse of a matrix
## The first function 'makeCacheMatrix()' caches the matrix and
## creates functions which will be used by other function 'cacheSolve()'
## to create cache of inversed matrix.

## Function 'makeCacheMatrix()' takes matrix as argument and cache it
## in some new variable (i.e. 'mat <- makeCacheMatrix(matrix)'),
## creates global environment variable 's' which has been set to NULL
## value and creates list of 4 useful functions, out of which 2 will be 
## used in the next function. Agruments for each function defined inside 
## this function have to be explicitly passed when function is called.

makeCacheMatrix <- function(x = matrix()) {
        s <<- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) s <<- inverse
        get_inv <- function() s
        list(set = set, get = get, set_inv = set_inv, 
             get_inv = get_inv)
}

## Function 'cacheSolve()' use as an argument variable previously 
## created in'makeCacheMatrix()' function. It calls 'get_inv()'
## function to store inversed matrix into global environment 
## variable 's', but since it has not been inversed before, with
## the function 'set_inv()' value of the variable 's' remains NULL
## Next thing this function does is to test if variable 's' is
## NOT NULL (just for the case that this function has previously stored
## inverse matrix), and if logical statement is TRUE it returns message
## 'getting cached data' together with inversed matrix stored in 's'
## If the statement is not TRUE, function continues with getting 
## original matrix and with function 'get()' and store it in 'data'
## variable. Than this matrix has been inversed and assigned to 's'.
## The last thing this function does is to cache inversed matrix
## by calling 'set_inv()' function and passing to it variable 's'
## as an argument. At the very end function returns (prints out) 's'
## variable (inversed matrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$get_inv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$set_inv(s)
        s
}