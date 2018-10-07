## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 ## assign NULL to inv so that cacheSolve function will caculate inverse matrix
    set <- function(y) {            ## set a new matrix to x
        x <<- y
        inv <<- NULL
    }
        
    get <- function() x         ## directly print x
    setinverse <- function(inverse) inv <<- inverse     ## assign the inverse matrix to setinverse  
    getinverse <- function()inv     ## print inverse matrix
    
    list( set = set,        ## make a list of these value
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {        ## build a function to caculate and store inverse matrix
    inv <- x$getinverse()   ## if there is a inverse matrix , just put it in the variable "inv" and print out.
    if(!is.null(inv)) {         ## if it doesn't exist yet, let's go on following steps.
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()      ## extract the original matrix out and assign to "mat"
    inv <- solve(mat, ...)      ## caculate inverse matrix and make a new variable for it
    x$setinverse(inv)       ## store tha inverse matrix to function -- "makeCacheMatrix"
    inv     ## return a inverse matrix of "x"
    
      
}
