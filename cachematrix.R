## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL #initialize inverse
      set <- function(y) {
          x <<- y # set matrix y to parent env x
          i <<- NULL # inverse is reset
      }
      get <- function() x  #assigns matrix x to 'get'
      #assign inverse to the cache
      setinv <- function(inv) i <<- inv
      #assign the cached value to getinv
      getinv <- function() i 
      list(set=set, get=get, setinv=setinv, getinv=getinv)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #set i to the inverse of x
        i <- x$getinv()
        
        #check the cached value, if calculated then present value
        if(!is.null(i)) {
            message ("getting cached data")
            return(i)
        } 
        
        #calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        
        #set value to inverse of original
        x$setinv(i)
        i
}
