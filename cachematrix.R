##########################################################
# 13/12/2014 - Proggramming Assignment2 - R Programming
# Coursera - Data Science Specialization
# the below functions creates a special "matrix" object 
# that can cache its inverse, and computes the inverse of 
# the special "matrix", if not already computed.
##########################################################


## makeCacheMatrix 
## This function shall,
## -Set A New Matrix, -Get Cached Matrix Value
## -Invert a given Matrix, -get the nverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #the inverse matrix in NULL if not already solved.
  inv <- NULL
  
  #Seting the matrix, 
  #make the inverse value NULL, 
  #when seting(new values) the matrix.
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #function snipet to read the set matrix
  getmat <- function() x
  
  #function parameter should be solve(solved).
  setinv <- function(solved) inv <<- solved
  
  #get the inveted matrix
  getinv <- function() inv 
  
  #assign names to the function snipets
  list(set = setmat, get = getmat, 
       setinv = setinv, getinv = getinv)
  
}


## cacheSolve 
## This function shall,
## compute the invert function, only if already not computed
## if alreday computed , retirive the data from cache memory.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get inverted matrix value
  inv  <- x$getinv()
  
  #if the matrix not null read it from 
  #cache memory, instead of computing
  if(!is.null(inv)) {
    message("getting cached data")
    
    #the function shall exit
    return(inv)
  }
  
  #if inv value in NULL,
  #get the matrix value
  data <- x$get()
  
  #solve the invert matrix
  #& save value in inv
  x$setinv(solve(data))
  
  #get the invert matrix
  x$getinv()
}
