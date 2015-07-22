## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix(): 
##   creates special object contains matrix and its inversed matrix in cache 
##   also has a list of below 4 functions;
##     get(): returns cached matrix 
##     set(x): set matrix x to cache
##     getInverse(): returns inversed matrix for cached matrix 
##     setInverse(inv): set inversed matrix to cache
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialization of inversed matrix
        i <- NULL;
        
        ## set(y): set matrix y to cached matrix x 
        set <- function(y){
                x <<- y
                i <<- NULL ## Initialization of inversed matrix
        }
        
        ## get(): returns cached matrix x
        get <- function() x
        
        ## setInverse(inv): set inversed matrix inv to cached matrix i
        setInverse <- function(inv) i <<- inv
        
        ## getInverse(): returns cached inversed matrix i
        getInverse <- function() i
        
        ## return the object in form of a list of 4 functions defined above
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve(x, ...)
##   returns inversed matrix for cache matrix x (created by makeCacheMatrix()).  
##   If the inversed matrix is already cached, just returns cached one.
##   If not cached, calculates inversed matrix and return it as well as store inversed matrix to cache matrix x.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## see if inversed matrix is already calculated and cached
        ## if it's cached, it just return it
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if it's not cached, then calculate inverse matrix and return it
        ## get matrix from cache
        matrix <- x$get()
        ## calculate inverse matrix
        inv <- solve(matrix, ...)
        ## set calculated inverse matrix to cache
        x$setInverse(inv)
        ## return calculated inverse matrix
        inv
}
