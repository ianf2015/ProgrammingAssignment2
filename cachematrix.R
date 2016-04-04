##Doing Matrix inversion by caching the inverse of a matrix rather 
##than compute it repeatedly.

##creates a special "matrix" object in order to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   ## set the value of the matrix
   set <- function(y) {  
      x <<- y
      m <<- NULL
   }
   
   ##get the value of the matrix
   get <- function() x   
   
   ##set the value of the inverse of the matrix
   setinversematrix <- function(solve) m <<- solve  
   
   ##get the value of the inverse of the matrix
   getinversematrix <- function() m   
   list(set = set, get = get,
        setinversematrix = setinversematrix,  
        getinversematrix = getinversematrix)  
}

## Computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   ## Check if there is a cached result and return it
   ## if not, solve for the inverse
   m <- x$getinversematrix()
   if(!is.null(m)) {    
      message("getting cached data")
      return(m)
   }else{ 
      data <- x$get()
      m <- solve(data,...)
      x$setinversematrix(m)
      m
   }
}