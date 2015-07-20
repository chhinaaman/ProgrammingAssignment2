## We are writing a R program to R function to test cache functionality for potentially time-consuming operations.
## We will be calculating inverse of a Matrix to test how caching works

## The makeCacheMatrix function will create a vector of functions to operate on the matrix we want to inverse

makeCacheMatrix <- function( x = matrix() ) {

    cachedInvMatrix <- NULL
  
    set <- function(y) {    #Sets the original Matrix values in cache
           x <<- y
           cachedInvMatrix <<- NULL
    }
          
  	get <- function() x  #gets the original matrix
  
  	#store the Inverse of the matrix in the cache
    setInvMatrix <- function(InvMatrix) cachedInvMatrix <<- InvMatrix
    
    #get the cached value of the inverse of the matrix
    getInvMatrix <- function() cachedInvMatrix
  
   list(set = set, get = get,
        setInvMatrix = setInvMatrix,
        getInvMatrix = getInvMatrix )
}

cacheSolve <- function(x) {

      
        cachedInvMatrix <- x$getInvMatrix()
        
        #Check if the cache value exists for the inverse matrix
        if(!is.null(cachedInvMatrix)) {
                message("getting cached inverse matrix")
                return(cachedInvMatrix)
        }

        #get the original matrix value
        mymatrix <- x$get()
        
        #use solve method to get the inverse of the matrix
        cachedInvMatrix  <- solve(mymatrix)
        
        #store the inverse matrix in the cache variable
        x$setInvMatrix(cachedInvMatrix)
        
        cachedInvMatrix
}

