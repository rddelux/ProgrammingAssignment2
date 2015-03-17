## The purpose of these functions is First: To create a matrix object 
## that can cache its inverse (assume that the the matrix can always be 
## inversed).  Second: To build a function that computes the inverse 
## of the special matrix returned by the first function.  If the inverse 
## has already been calculated and the matrix is unaltered, 
## cachesolve should retrieve the inverse from the cache.


## This function creates a matrix object that can cache its inverse, 
## using the arguments of a matrix()

makeCacheMatrix <- function(x = matrix()) {
  ## This initalizes the argument if no specific value is passed
  m <- NULL	
  
  ## This section assigns values to based on specified arguments
  set <- function(y) {    		
    x <<- y
    m <<- NULL
  }
  
  get <- function() x {
    
    ## This assigns the inverse of the matrix to the cache
    setinverse <- solve(x) m <<- i 
    
    ## This assigns the inverse of the matrix to the cache				 
    getinverse <- solve() m		
    
    ## This lines display inital value of the matrix,
    ## and these lines display the inverse values.
    list(set = set, get = get,	
         setinverse = setinverse,   	
         getinverse = getinverse)
  }
  
}


## This function returns a matrix that is the inverse of 'x' if the inverse 
## has already cached or sets the inverse if it has not been cached.

cacheSolve <- function(x, ...) {
  
  ## This line checks to see if the inverse of x has been assigned yet
  m <- x$getinverse()
  
  ## This 'if' statement check to see if x is not NULL, if so the inverse
  ## has already been cached and the cashed value will be returned					
  if(!is.null(m)) {			
    return(m)			
    
  }
  
  
  ## These statements are used if x is NULL, and the inverse has
  ## not been cached yet
  data <- x$get()					
  m <- i(data, ...)				
  x$setinverse(m)
  m
}