## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

 ## Return a list of the methods
 list(set = set, get = get,
         setInverse = setInverse,
        getInverse = getInverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
