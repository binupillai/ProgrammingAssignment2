##########################################
#function to cache matrix                #
#                                        #
#version : 1.0                           #
#                                        #
#date : 06/22/2014                       #
#                                        #
#author : Binu Pillai                    #
##########################################

makeCacheMatrix <- function(x = matrix()){
  
  m  <- NULL      ##initializing local function variable
  
  ##setting local function variable (x) to new value when the new value is not same as existing 
  set <- function(y) {
    
    ##Check whether the new matrix (y) is different from current (x) 
    if (is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)) {
      
      message("Identical matrix , no need to set the local variable again and re-calculate.")
      
    } else { ## if X and y are not same assign x to y and set m to NULL
      
      x <<- y
      
      m <<- NULL
    }
  }
  
  ## return value of x
  get <- function() x
  
  ##set m
  setCacheMatrix <- function(cm) m <<- cm
  
  ##return m
  getCacheMatrix <- function() m
  
  list (set = set , get = get , setCacheMatrix = setCacheMatrix,getCacheMatrix = getCacheMatrix)
  
}


##########################################
#function to calculate incerse of matrix #
#                                        #
#version : 1.0                           #
#                                        #
#date : 06/22/2014                       #
#                                        #
#author : Binu Pillai                    #
##########################################

cacheSolve <- function(x, ...){
  
  ##get cached matrix
  m <- x$getCacheMatrix()
  
  if (!is.null(m)){ ##if m is not null then m is cached, retun value of m
    
    message("Getting cached data ....")
    
    return(m)
    
  } 
  
  data <- x$get()
  
  ##as per requerements assuming that the matrix supplied is invertible. 
  m <- solve(data)
  
  x$setCacheMatrix(m)
  
  m
}
