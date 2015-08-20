## function 1: create special matrix object capable of caching its own matrix inversion
## function 2: resolve matrix inversion from special matrix object

## Takes an optional matrix parameter and returns an object with four functions: setMatrix, getMatrix, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
     invm = NULL
     setMatrix <- function(m) {
          x <<- m
          invm <<- solve(m)
     }
     getMatrix <- function() x
     setInverse <- function(m) {
          x <<- m
          invm <<- solve(m)
     }
     getInverse <- function() invm
     list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## takes a makeCacheMatrix object parameter and returns it's inverse matrix if cached, otherwise sets & caches it's inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invm <- x$getInverse()
     if(is.null(invm)) {
          m <- x$getMatrix()
          x$setInverse(m)
          invm <- x$getInverse()
     } 
     return(invm)
}
