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
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
 ## This function creates a special "matrix" object that can cache its inverse.
 ## Put comments here that give an overall description of what your
## functions do
 ## Write a short comment describing this function
 makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
    }
        
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function()inv
    
    list( set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
 }
 ## Write a short comment describing this function
 cacheSolve <- function(x,...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setinverse(inv)
    inv
    
        ## Return a matrix that is the inverse of 'x'
}
 my_matrix <- makeCacheMatrix(x = matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$set()
my_matrix$setinverse()
my_matrix$getinverse()
 cacheSolve(my_matrix)
 my_matrix$getinverse()
 my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
 my_matrix$getinverse()
 cacheSolve(my_matrix)
 my_matrix$getinverse()
my_matrix$setinverse() 
