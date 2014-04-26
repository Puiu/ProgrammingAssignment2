## Put comments here that give an overall description of what your
## functions do


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## will use this to store and set the original
        obj <- NULL 
        
        getData<- function() x ## returns the original
       
        #sets the inverse to an internal object
        setInverse <- function(s) obj <<- s 
        
        #gets the inverse
        getInverse <- function() obj
        
        #defines the object returned
        list(getData = getData,
             setInverse = setInverse,
             getInverse = getInverse)
                
}



## Computes the inverse of a matrix. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
               
        #try to get the inverse from cache
        newMatrix <- x$getInverse()
        
        #check to see if the cache contains anything
        if(!is.null(newMatrix))
        {
                #bingo! we have a matrix in the cache
                message("getting cached matrix")
                return(newMatrix)
        }
        
        #there is no matrix in the cache
        #first we get the original matrix
        data <- x$getData()
        
        #create the inverse
        data <- solve(data)
        
        #store the inverse in the cache
        x$setInverse(data)
        
        #show the inverse
        data
}
