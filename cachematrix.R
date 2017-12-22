### R Progamming - Week 3 Assignment
### The goal of this exercise it to create a process to calculate and store the inverse of a matrix in the 
### cache / parent environment, and use it for subsequent process/calculations.


#################################
### Function: makeCacheMatrix ###
#################################

### This function returns a list of functions that can 
### set and get 
### a matrix and its inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
     
     ## x <- matrix()         ## not necessary, as it it passed as the argument
     ix   <- matrix()         ## initialize an empty matrix for x's inverse
     
     
     ## initialize/set the (orig) matrix
     setMatrix <- function(m1) {
          
          x <<- m1                 ## save the passed (orig) matrix to cache/parent env
          ix <<- matrix()          ## clear the cache for the inverse matrix
     }
     
     
     ## get the (orig) matrix
     getMatrix <- function() {
          x                        ## returns the orig matrix 
     }
     
     
     
     ## set the inverse of (orig) matrix
     setInvMatrix  <- function(m2) {
          ix <<- m2
     }
     
     
     
     ## get the inverse of (orig) matrix
     getInvMatrix  <- function() {
          ix 
     }
     
     
     ## return a list of the above 4 functions
     list(setMatrix = setMatrix,
          getMatrix = getMatrix,
          setInvMatrix = setInvMatrix,
          getInvMatrix = getInvMatrix)
}




############################
### Function: cacheSolve ###
############################

### This function returns the inverse of the passed matrix,
### and saves the result in the cache 

cacheSolve <- function(x, ...) {
     
     ### first, check to see if the inverse of the matrix is available in the cache
     im <- x$getInvMatrix()         
     
     if( !is.na(im)) {         ## is the inverse-matrix in cache not empty?
          
          message ("Geting cached data...")
          return (im)     ## return the cached inverse-matrix
     }
     
     
     
     ### Reset the cache with the new matrix and its Inverse matrix
     
     m <- x$getMatrix()       ## get orig matrix
     
     im <- solve(m)           ## calculate the inverse of the orig matrix
     
     x$setInvMatrix(im)       ## store the newly calculated inv-matrix to cache
     
     
     ### Return the inv-matrix
     im
     
}