## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse. It will set the value of the matrix,
## and then get the value of the matrix, set and get the inverse Matrix.


## the matrix is considered to be the input

makeCacheMatrix <- function(x = matrix()) {
 invmatrix <- NULL
 setmatrix <- function(y) {
  x <<- y
  invmatrix <<- NULL
   }
 getmatrix <- function() x                             
 setinverse <- function(inverse) invmatrix <<- inverse  
 getinverse <- function() invmatrix                   
 list(setmatrix = setmatrix, 
      getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


##The function cacheSolve: the output of the makeCacheMatrix is the input of cacheSolve
##and the function also checks if inverse matrix from makeCacheMatrix) has any value in.

cacheSolve <- function(x, ...) {
   invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {                               
 message("Getting Cached Invertible Matrix")            
 return(invmatrix)                                    
 }
 MatrixData <- x$getmatrix()                            
 invmatrix <- solve(MatrixData, ...)       
 x$setinverse(invmatrix)                         
 invmatrix
        ## Return a matrix that is the inverse of 'x'
}
