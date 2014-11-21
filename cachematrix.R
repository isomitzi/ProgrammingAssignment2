## what i did was to mimic the thought pattern that was shown in the example
## the first function will hold a place for the inverse matrix as well as nested function
## the nested function will be able to get the original matrix, set it, get the inverse matrix
##and set the inverse matrix.
## the second function will check for a chached value. if it's there- the funtion will return it
## else, the function will calculate the inverse matrix, store it in first function and will print it.

## a function to hold the matrix as well as a "getter" and "setter" function for the matrix and the inversed value

makeCacheMatrix <- function(x = matrix()) {
  
      inv_val <- NULL ## setting the inverse value to NULL
      
      set <- function(y) {        ## the "setter" function- setting a new matrix and reseting the inv_val value
            x <<- y 
            inv_val <<- NULL  
      } 
      
      get <- function() {x}     ## the "getter" function -retrieves the original matrix
      
      setinv <- function(inv) { inv_val <<- inv}   ## a function for setting the cached inverse value
      
      getinv <- function() {inv_val}  ## a function for getting the inversed value 
      
      list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## a function to Return a matrix that is the inverse of 'x'. will use cached value (if such exist)

cacheSolve <- function(x, ...) {
  
    cache_val <- x$getinv()   ##getting the cached value
   
    if(!is.null(cache_val)) {                   ## if the cached value exists, the function simply returns it
      
           message("getting cached data")
          
           return(cache_val)
    }
    
    data <- x$get()                     ## it there isn't a cached value, we get the matrix
    
    inv_val <- solve(data, ...)         ## we calculate the inversed value
    
    x$setinv(inv_val)                   ## we store it in the cache
    
    inv_val                             ##and we return it
  
        
}
