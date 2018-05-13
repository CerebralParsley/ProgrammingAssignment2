## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## We first define the argument
                                            ## of the function and set it to default "matrix"
  

  invM <- NULL                              ## We then initialize invM as NULL, 
                                            ## and this will hold the matrix inverse  
  setM <- function(y) {                     ## We set the function to assign new
    x <<- y                                 ## This is the value of matrix in parent environment
    invM <<- NULL                           ## If there is a new matrix, we reset the invM to NULL
  }
  getM <- function() x                      ## We define the function getM that returns the value
                                            ## of the matrix argument
  
  setinvM <- function(inverse) invM <<- inverse ## assigns the value of invM in parent environment
  getinvM <- function() invM                    ## gets the value of invM when called
  
  list(setM = setM, getM = getM, setinvM = setinvM, getinvM = getinvM)   ## this is needed to refer to 
                                                                         ## the functions with $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinvM()
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
data <- x$getM()
inv <- solve(data, ...)
x$setinvM(invM)
invM
}
