## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

 i <- NULL
 
 ## Method to set the matrix
 set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
         }

 ## Return matrix when called
 get <- function() {
         m
         }

 ## Inverse the input matrix
 setInverse <- function(inverse) {
        i <<- inverse
 }

 ## Return the inversed matrix
 getInverse <- function() {
        i
}

 ## Return the list of methods 
 list(set = set, get = get,
         setInverse = setInverse,
        getInverse = getInverse)

}

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()

## return if inverse is calculated
        f( !is.null(m) ) {
          return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        return(m)
        }
        
