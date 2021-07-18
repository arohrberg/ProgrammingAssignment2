## makes matrix to cache inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function(){x}
      setInv <- function(inverse) {
        inv <<- inverse
      }
      getInv <- function() {inv}
      
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## check if cache is inverse, return if so, else do inverse input matrix and return said inverse

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv))  {
          print("cache found")
          return(inv)
        }
        z <- x$get()
        inv <- solve(z , ...)
        x$setInv(inv)
        inv
}

## test
## it took me days and probably a lot of gray hairs to remember that zero-determinant matrices can never be inversed...
## grabMatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)) 
## works
## grabMatrix$get() 
## check if in cache
## grabMatrix$getInv() ## NULL
## cacheSolve(grabMatrix) ## inv 
## cacheSolve(grabMatrix) ## print works as expected
