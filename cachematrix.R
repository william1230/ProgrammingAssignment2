## There are two functions below, one to create a special "matrix" which returns
## a list of 4 functions. Another one to compute the inverse of the special "matrix"
## where it will return the result from the cache if it's already computed earlier


## makeCacheMatrix creates a special "matrix" which has a list
## containing functions to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve calculates the inverse of the special "matrix"
## If the inversed matrix has been calculated, the function will take from the cache
## and skips the computation
## Otherwise, it computes the inverse of the matrix and sets the results in the
## cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
